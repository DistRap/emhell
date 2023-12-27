{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Exception (SomeException)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get, put)
import Data.Default.Class (Default(def))
import Data.SVD.Types (Device)
import Data.Word (Word32)
import EmHell.SVD.Selector (Selector(..))
import Gdb (Config(..), GDBT)
import System.Console.Repline
  ( CompletionFunc
  , HaskelineT
  , MultiLine(..)
  , CompleterStyle(Prefix)
  , ExitDecision(Exit)
  )

import qualified Control.Concurrent
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified Data.SVD.Util
import qualified EmHell.SigintHandler
import qualified EmHell.SVD.Completion
import qualified EmHell.SVD.Selector
import qualified Gdb
import qualified System.Console.Repline
import qualified System.Directory

import Options

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
        then Gdb.armConfig
        else def
      cfg =
        case gdbConfig of
          c@Config {} ->
            c { confLogfile = optsMILog opts }
          c@ConfigTCP {} ->
            c { confTCPLogfile = optsMILog opts }

  case optsCwd opts of
    Nothing -> pure ()
    Just pth ->
      System.Directory.setCurrentDirectory pth

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

  _ <- Gdb.runGDBConfig cfg $ do
    maybe (pure ()) Gdb.file (optsFile opts)
    maybe (pure ()) Gdb.extRemote (optsProg opts)
    -- execute extra --ex cli commands
    Control.Monad.forM_ (optsEx opts) Gdb.cli

    Gdb.break

    -- to avoid printing prompt during gdb log output
    liftIO
      $ Control.Concurrent.threadDelay 1000000
    Control.Monad.void
      $ (`runStateT` dev)
      $ runRepl

  pure ()

runRepl :: StateT (Maybe Device) (GDBT IO) ()
runRepl = do
  System.Console.Repline.evalRepl
    banner'
    (replCmd)
    options
    (Just ':')
    (Just "paste")
    (Prefix
      (\x ->
        ( EmHell.SVD.Completion.compFunc
          svdCompleterMay
        )
        x
      )
      defaultMatcher
    )
    greeter
    finalizer
  where
    banner' =
        pure
      . \case
          SingleLine -> "hgdb> "
          MultiLine -> "| "

    greeter =
      liftIO
      $ putStrLn "Welcome to hgdb"

    finalizer = pure Exit

    svdCompleterMay
      :: Monad m
      => String
      -> StateT (Maybe Device) m [String]
    svdCompleterMay x = do
      s <- get
      case s of
        Nothing -> pure mempty
        Just dev ->
          EmHell.SVD.Completion.svdCompleter dev x

replCmd :: String -> Repl ()
replCmd input = lift $ do
  svdMay <- get
  case svdMay of
    Nothing -> lift $ Gdb.cli input
    Just dev -> do
      case EmHell.SVD.Selector.parseSelector input of
        Left _e -> do
          lift $ Gdb.cli input
        Right sel -> do
          case Data.SVD.Util.getPeriphRegAddr (selPeriph sel) (selReg sel) dev of
            Left e -> lift $ Gdb.echo e
            Right regAddr -> do
              res <- lift $ Gdb.readMem regAddr 4
              case res of
                Nothing -> error "Failed to read memory via GDB"
                Just x -> do
                  case
                    Data.SVD.Util.getPeriphReg
                      (selPeriph sel)
                      (selReg sel)
                      dev
                    of
                      Left _e -> error "Absurd"
                      Right reg ->
                        liftIO
                          $ Data.SVD.Pretty.Explore.exploreRegister
                              (x :: Word32)
                              regAddr
                              reg

wait :: String -> Repl ()
wait _args = liftGdb $ Gdb.waitStop >>= Gdb.showStops

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
interruptible
  :: Show b
  => GDBT IO b
  -> a
  -> Repl ()
interruptible act _args = do
  ctx <- liftGdb Gdb.getContext
  x <-
    liftIO
      $ Control.Exception.try
      $ EmHell.SigintHandler.sigintHandler
      $ Gdb.runGDBT ctx act
  _ <- case x of
    Left e -> do
      -- if user hits Ctr-C we propagate it to GDB and wait for stops response
      liftGdb Gdb.break
      -- this used to be, but why
      -- wait []
      pure $ Left $ show (e :: SomeException)
    Right r -> pure $ Right r

  pure ()

defaultMatcher :: [(String, CompletionFunc (StateT (Maybe Device) (GDBT IO)))]
defaultMatcher =
  [ (":svd", System.Console.Repline.fileCompleter)
  , (":file", System.Console.Repline.fileCompleter)
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
  , ("file", liftGdb . Gdb.file)
  , ("wait", wait)
  , ("c", interruptible $ Gdb.continue >> Gdb.waitStop)
  ]
