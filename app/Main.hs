{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (break)

import Data.Bits
import Data.Word
import Text.Printf

import Control.Concurrent (threadDelay, myThreadId, throwTo, forkIO)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import System.Console.Repline
import System.Console.Haskeline.MonadException hiding (throwTo)
import System.Directory

import Gdb
import Data.SVD
import Data.Bits.Pretty

import Box
import Completion
import Options
import Selector

type Repl a = HaskelineT (StateT (Maybe Device) (GdbT IO)) a

prettyRegister :: (PrintfArg a, FiniteBits a, Show a, Integral a)
               => a
               -> Int
               -> Register
               -> IO ()
prettyRegister x addr reg = do
  putStrLn $ "Register " ++ regName reg
  putStrLn $ "- " ++ regDescription reg
  let a = showHex (fromIntegral addr :: Word32)
      b = showHex (fromIntegral (regAddressOffset reg) :: Word8)
  putStrLn $ "- Address " ++ a ++ " (including offset " ++ b ++ ")"
  putStrLn ""

  case x of
    0 -> putStrLn "(Just zeros)"
    _ -> do

      putStrLn $ showDec x
      putStrLn $ showHex x
      putStrLn $ showBin x
      putStrLn $ "0b" ++ showBinGroups 4 x

      putStrLn $ printSetFields $ getProcdFieldValues x reg

  putStrLn ""
  renderFields $ getProcdFieldValues (x) reg

main = do
  opts <- runOpts

  let gdbConfig = if optsArm opts then armConfig else defaultConfig
      cfg = gdbConfig { confLogfile = optsMILog opts }

  case optsCwd opts of
    Nothing -> return ()
    Just pth -> setCurrentDirectory pth -- "/home/srk/git/ivory-tower-helloworld"

  dev <- case optsSVD opts of
    Nothing -> return Nothing
    Just fp -> do
      liftIO $ putStrLn $ "Loading SVD file " ++ fp
      x <- parseSVDPeripherals fp
      case x of
        Left err -> putStrLn err >> return Nothing
        Right s -> return $ Just $ defaultDevice { devicePeripherals = s }


  runGdbConfig cfg $ do
    maybe (return ()) file (optsFile opts)
    maybe (return ()) extRemote (optsProg opts)
    forM_ (optsEx opts) $ \x -> cli x

    --breakpoint $ C.function_location "callback_extint_handle_4_thread_signal_EXTI15_10_IRQHandler"
    --run
    {--
    forever $ do
      onBreak $ \b -> do
        val <- eval "xcnt_4"
        echo val
    --}
    break

    -- to avoid printing prompt during gdb log output
    liftIO $ threadDelay 1000000
    void $ flip runStateT dev $ runRepl


runRepl :: StateT (Maybe Device) (GdbT IO) ()
runRepl = do
    evalRepl (pure "λ> ")
      (replCmd)
      options
      (Just ':')
      (Prefix (\x -> (compFunc $ svdCompleterMay) x ) (defaultMatcher))
      (return ())

replCmd :: String -> Repl ()
replCmd input = lift $ do
  svdMay <- get
  case svdMay of
    Nothing -> lift $ cli input
    Just dev -> do
      case parseSelector input of
        Left _e -> do
          lift $ cli input
        Right sel -> do
          case getPeriphRegAddr (selPeriph sel) (selReg sel) dev of
            Left e -> lift $ echo e
            Right regAddr -> do
              (Just x) <- lift $ readMem regAddr 4
              let Right reg = getPeriphReg (selPeriph sel) (selReg sel) dev
              liftIO $ prettyRegister (x :: Word32) regAddr reg

wait :: [String] -> Repl ()
wait _args = liftGdb $ waitStop >>= showStops

loadSVD :: [String] -> Repl ()
loadSVD [fp] = do
  liftIO $ putStrLn $ "Loading SVD file " ++ fp
  x <- liftIO $ parseSVDPeripherals fp
  case x of
    Left err -> liftIO $ putStrLn err
    Right s -> lift $ put $ Just $ defaultDevice { devicePeripherals = s }
loadSVD _ = liftIO $ putStrLn "Requires path to SVD file"

-- does make sense only when GDB is running, add continue >> act??
interruptible :: Show b => GdbT IO b -> a -> Repl ()
interruptible act _args = do
  ctx <- liftGdb ask
  x <- liftIO $ E.try $ sigintHandler $ flip runReaderT ctx act
  _ <- case x of
    Left e -> do
      -- if user hits Ctr-C we propagate it to GDB and wait for stops response
      liftGdb break
      wait []
      return $ Left $ show (e :: SomeException)
    Right r -> return $ Right r

  return ()

defaultMatcher :: [(String, CompletionFunc (StateT (Maybe Device) (GdbT IO)))]
defaultMatcher =
  [ (":svd", fileCompleter)
  , (":file", fileCompleter)
  ]

liftGdb :: (MonadTrans t1, MonadTrans t2, Monad m, Monad (t2 m))
        => m a
        -> t1 (t2 m) a
liftGdb fn = lift . lift $ fn

fileArg fn [x] = liftGdb $ fn x
fileArg fn _ = liftGdb $ echo "Requires FILE argument"

options :: [(String, [String] -> Repl ())]
options = [
    ("svd", loadSVD)
  , ("file", fileArg $ file)
  , ("wait", wait)
  , ("c", interruptible $ continue >> waitStop)
  ]
