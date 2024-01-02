{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.SVD.Types (Device)
import Data.Word (Word32)
import EmHell.SVD.Selector (Selector(..))
import System.Console.Repline
  ( HaskelineT
  , MultiLine(..)
  , CompleterStyle(Prefix)
  , CompletionFunc
  , ExitDecision(Exit)
  )

import qualified Control.Monad
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified EmHell.Options
import qualified EmHell.SVD.Completion
import qualified EmHell.SVD.Query
import qualified EmHell.SVD.Manipulation
import qualified EmHell.SVD.Selector
import qualified System.Console.Repline

import Options.Applicative

type Repl a =
  HaskelineT
    (ReaderT Device IO) a

main :: IO ()
main = do
  fp <- runOpts

  liftIO
    $ putStrLn
    $ "Loading SVD file " <> fp

  x <- Data.SVD.IO.parseSVD fp
  dev <- case x of
    Left err -> error err
    Right dev -> pure dev

  Control.Monad.void
    $ (`runReaderT` dev)
    $ runRepl

runRepl :: ReaderT Device IO ()
runRepl = do
  System.Console.Repline.evalRepl
    banner'
    (replCmd)
    options
    (Just ':')
    (Just "paste")
    (Prefix
      ( EmHell.SVD.Completion.compFunc
        $ (ask >>=)
        . flip EmHell.SVD.Completion.svdCompleter
      )
      defaultMatcher
    )
    greeter
    finalizer
  where
    banner' =
        pure
      . \case
          SingleLine -> "emhell> "
          MultiLine -> "| "

    greeter =
      liftIO
      $ putStrLn "Welcome to emhell"

    finalizer = pure Exit

replCmd :: String -> Repl ()
replCmd input = lift $ do
  dev <- ask
  case EmHell.SVD.Selector.parseSelector input of
    Left _e -> pure ()
    Right sel ->
      case
          EmHell.SVD.Query.getRegWithAddr
            (selPeriph sel)
            (selReg sel)
            dev
        of
          Right (reg, regAddr) ->
            liftIO
              $ Data.SVD.Pretty.Explore.exploreRegister
                  (0 :: Word32)
                  regAddr
                  reg

          Left e -> liftIO $ putStrLn e

defaultMatcher :: [(String, CompletionFunc (ReaderT Device IO))]
defaultMatcher =
  [ ( ":set"
    , EmHell.SVD.Completion.compFunc
        $ (ask >>=)
        . flip EmHell.SVD.Completion.svdCompleterFields
    )
  ]

options :: [(String, String -> Repl ())]
options = [
    ("set", setReg)
  ]

setReg :: String -> Repl ()
setReg input = lift $ do
  dev <- ask
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
            let eNewVal = case selField sel of
                  Just f ->
                    EmHell.SVD.Manipulation.setField reg 0 f v
                  Nothing ->
                    pure v
            case eNewVal of
              Left e -> liftIO $ putStrLn e
              Right newVal ->
                liftIO
                  $ Data.SVD.Pretty.Explore.exploreRegister
                      (newVal :: Word32)
                      regAddr
                      reg

          Left e -> liftIO $ putStrLn e

runOpts :: IO FilePath
runOpts =
  execParser
  $ info
      (EmHell.Options.parseSVD <**> helper)
      (fullDesc <> progDesc "emhell")
