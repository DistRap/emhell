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
  , ExitDecision(Exit)
  )

import qualified Control.Monad
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified Data.SVD.Util
import qualified EmHell.Options
import qualified EmHell.SVD.Completion
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
    mempty
    (Just ':')
    (Just "paste")
    (Prefix
      (\x ->
        ( EmHell.SVD.Completion.compFunc
          (\i -> ask >>= \dev -> EmHell.SVD.Completion.svdCompleter dev i)
        )
        x
      )
      mempty
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
        ( Data.SVD.Util.getPeriphRegAddr
            (selPeriph sel)
            (selReg sel)
            dev
        , Data.SVD.Util.getPeriphReg
            (selPeriph sel)
            (selReg sel)
            dev
        )
        of
          (Right regAddr, Right reg) ->
            liftIO
              $ Data.SVD.Pretty.Explore.exploreRegister
                  (0 :: Word32)
                  regAddr
                  reg
          _ -> error "Absurd"

runOpts :: IO FilePath
runOpts =
  execParser
  $ info
      (EmHell.Options.parseSVD <**> helper)
      (fullDesc <> progDesc "emhell")
