{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Prettyprinter ((<+>), annotate, pretty)
import Prettyprinter.Render.Terminal (Color(..), bold, color)

import qualified Control.Concurrent
import qualified Control.Exception
import qualified Control.Monad
import qualified Data.Bits.Pretty
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified Data.Text.IO
import qualified EmHell.SigintHandler
import qualified EmHell.SVD.Completion
import qualified EmHell.SVD.Query
import qualified EmHell.SVD.Manipulation
import qualified EmHell.SVD.Selector
import qualified Gdb
import qualified Prettyprinter
import qualified Prettyprinter.Render.Terminal
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
    completion
    greeter
    finalizer
  where
    banner' =
        pure
      . \case
          SingleLine -> "hgdb> "
          MultiLine -> "| "

    options :: [(String, String -> Repl ())]
    options = [
        ("set", setReg)
      , ("svd", loadSVD)
      , ("file", liftGdb . Gdb.file)
      , ("wait", wait)
      , ("c", interruptible $ Gdb.continue >> Gdb.waitStop)
      ]

    completion :: CompleterStyle (StateT (Maybe Device) (GDBT IO))
    completion =
      Prefix
        (EmHell.SVD.Completion.compFunc
           svdCompleterMay
        )
        defaultMatcher

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

    svdCompleterFieldsMay
      :: Monad m
      => String
      -> StateT (Maybe Device) m [String]
    svdCompleterFieldsMay x = do
      s <- get
      case s of
        Nothing -> pure mempty
        Just dev ->
          EmHell.SVD.Completion.svdCompleterFields dev x

    defaultMatcher :: [(String, CompletionFunc (StateT (Maybe Device) (GDBT IO)))]
    defaultMatcher =
      [ ( ":set"
        , EmHell.SVD.Completion.compFunc
            svdCompleterFieldsMay
        )
      , (":svd", System.Console.Repline.fileCompleter)
      , (":file", System.Console.Repline.fileCompleter)
      ]

    greeter =
      liftIO
      $ putStrLn "Welcome to hgdb"

    finalizer = pure Exit

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
          case
              EmHell.SVD.Query.getRegWithAddr
                (selPeriph sel)
                (selReg sel)
                dev
            of
              Left e -> lift $ Gdb.echo e
              Right (reg, regAddr) -> do
                res <-
                  lift
                    . Gdb.readMem32
                    . Gdb.memAddr
                    $ fromIntegral regAddr
                case res of
                  Nothing ->
                    lift $ Gdb.echo "Failed to read memory via GDB"
                  Just x -> do
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

liftGdb
  :: ( MonadTrans t1
     , MonadTrans t2
     , Monad m
     , Monad (t2 m)
     )
  => m a
  -> t1 (t2 m) a
liftGdb fn = lift . lift $ fn

setReg :: String -> Repl ()
setReg input = lift $ do
  svdMay <- get
  case svdMay of
    Nothing -> liftIO $ putStrLn "No SVD loaded"
    Just dev -> do
      case EmHell.SVD.Selector.parseSelectorValue input of
        Left e ->
          liftIO
            $ putStrLn
            $ "No parse " <> e
        Right (sel, v) ->
          case
              EmHell.SVD.Query.getRegWithAddr
                (selPeriph sel)
                (selReg sel)
                dev
            of
              Right (reg, regAddr) -> do
                let regMemAddr =
                      Gdb.memAddr
                      $ fromIntegral regAddr

                mOrigRegVal <-
                  lift $ Gdb.readMem32 regMemAddr

                case mOrigRegVal of
                  Nothing -> liftIO $ putStrLn "Unable to read memory"
                  Just origRegVal -> do
                    let eNewVal = case selField sel of
                          Just f ->
                            EmHell.SVD.Manipulation.setField
                              reg
                              origRegVal
                              f
                              v
                          Nothing ->
                            pure v
                    case eNewVal of
                      Left e ->
                        liftIO
                          . Data.Text.IO.putStrLn
                          . Prettyprinter.Render.Terminal.renderStrict
                          $ Prettyprinter.layoutPretty
                              Prettyprinter.defaultLayoutOptions
                              e
                      Right newVal -> do
                        liftIO
                          . Data.Text.IO.putStrLn
                          . Prettyprinter.Render.Terminal.renderStrict
                          $ Prettyprinter.layoutPretty
                              Prettyprinter.defaultLayoutOptions
                              (annotate
                                (bold <> color Green)
                                (   "Writing"
                                <+> pretty (Data.Bits.Pretty.formatHex newVal)
                                <+> "to"
                                <+> pretty (selPeriph sel)
                                <>  "."
                                <>  pretty (selReg sel)
                                )
                              )
                        lift
                          $ Gdb.writeMem32
                              regMemAddr
                              [newVal]

                        mNewRegVal <- lift $ Gdb.readMem32 regMemAddr
                        case mNewRegVal of
                          Nothing -> liftIO $ putStrLn "Unable to re-read memory"
                          Just newRegVal ->
                            liftIO
                              $ Data.SVD.Pretty.Explore.exploreRegister
                                  newRegVal
                                  regAddr
                                  reg

              Left e -> liftIO $ putStrLn e
