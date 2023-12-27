{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (break)

import Data.Bits
import Data.Default.Class
import Data.Word ()
import Text.Printf

import Control.Concurrent (threadDelay, forkIO)
import qualified Control.Exception as E
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader

import System.Console.Repline hiding (options)
import System.Directory

import Gdb
import Data.SVD.Types (Device)
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified Data.SVD.Util

import EmHell.SigintHandler
import EmHell.SVD.Completion
import EmHell.SVD.Selector

import Options

-- handler

type Repl a =
  HaskelineT
    (StateT (Maybe Device)
      (GDBT IO)
    ) a

main :: IO ()
main = do
  opts <- runOpts

  let gdbConfig =
        if optsArm opts
        then armConfig
        else def
      cfg =
        gdbConfig
          { confLogfile = optsMILog opts }

  case optsCwd opts of
    Nothing -> pure ()
    Just pth -> setCurrentDirectory pth

  dev <- case optsSVD opts of
    Nothing -> pure Nothing
    Just fp -> do
      liftIO
        $ putStrLn
        $ "Loading SVD file " <> fp

      x <- Data.SVD.IO.parseSVD fp
      case x of
        Left err ->
          putStrLn err >> pure Nothing
        Right dev ->
          pure $ pure dev

  runGDBConfig cfg $ do
    maybe (pure ()) file (optsFile opts)
    maybe (pure ()) extRemote (optsProg opts)
    -- execute extra --ex cli commands
    forM_ (optsEx opts) cli

    break

    -- to avoid printing prompt during gdb log output
    liftIO $ threadDelay 1000000
    void
      $ (`runStateT` dev)
      $ runRepl

  pure ()

runRepl :: StateT (Maybe Device) (GDBT IO) ()
runRepl = do
    evalRepl
      banner
      (replCmd)
      options
      (Just ':')
      (Just "paste")
      (Prefix (\x -> (compFunc $ svdCompleterMay) x ) (defaultMatcher))
      greeter
      finalizer
  where
    banner =
        pure
      . \case
          SingleLine -> "hgdb> "
          MultiLine -> "| "

    greeter =
      liftIO
      $ putStrLn "Welcome to hgdb"

    finalizer = pure Exit

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
          case Data.SVD.Util.getPeriphRegAddr (selPeriph sel) (selReg sel) dev of
            Left e -> lift $ echo e
            Right regAddr -> do
              res <- lift $ readMem regAddr 4
              case res of
                Nothing -> error "Failed to read memory via GDB"
                Just x -> do
                  case
                    Data.SVD.Util.getPeriphReg
                      (selPeriph sel)
                      (selReg sel)
                      dev
                    of
                      Left e -> error "Absurd"
                      Right reg ->
                        liftIO
                          $ Data.SVD.Pretty.Explore.exploreRegister 
                              (x :: Word32)
                              regAddr
                              reg

wait :: String -> Repl ()
wait _args = liftGdb $ waitStop >>= showStops

-- Repl command, that loads SVD
-- and puts @Device@ into repl state
loadSVD :: String -> Repl ()
loadSVD fp = do
  liftIO
    $ putStrLn
    $ "Loading SVD file " <> fp

  x <-
    liftIO
      $ Data.SVD.IO.parseSVD fp

  case x of
    Left err -> liftIO $ putStrLn err
    Right d -> lift $ put $ Just d

-- does make sense only when GDB is running, add continue >> act??
interruptible :: Show b => GDBT IO b -> a -> Repl ()
interruptible act _args = do
  ctx <- liftGdb getContext
  x <- liftIO $ E.try $ sigintHandler $ runGDBT ctx act
  case x of
    Left e -> do
      -- if user hits Ctr-C we propagate it to GDB and wait for stops response
      liftGdb break
      -- this used to be, but why
      -- wait []
      pure $ Left $ show (e :: E.SomeException)
    Right r -> pure $ Right r

  pure ()

defaultMatcher :: [(String, CompletionFunc (StateT (Maybe Device) (GDBT IO)))]
defaultMatcher =
  [ (":svd", fileCompleter)
  , (":file", fileCompleter)
  ]

liftGdb
  :: ( MonadTrans t1
     , MonadTrans t2
     , Monad m
     , Monad (t2 m)
     )
  => m a
  -> t1 (t2 m) a
liftGdb fn = lift . lift $ fn

options :: [(String, String -> Repl ())]
options = [
    ("svd", loadSVD)
  , ("file", liftGdb . file)
  , ("wait", wait)
  , ("c", interruptible $ continue >> waitStop)
  ]
