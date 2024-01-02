{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import Data.Default.Class (Default(def))
import Data.SVD.Types (Device)
import EmHell.SVD.Selector (Selector(..))
import HOCD (OCDT, OCDConfig(..))
import System.Console.Repline
  ( HaskelineT
  , MultiLine(..)
  , CompleterStyle(Prefix)
  , CompletionFunc
  , ExitDecision(Exit)
  )
import Prettyprinter ((<+>), annotate, pretty)
import Prettyprinter.Render.Terminal (Color(..), bold, color)

import qualified Control.Monad
import qualified Data.Bits.Pretty
import qualified Data.Maybe
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified Data.Text.IO
import qualified EmHell.Options
import qualified EmHell.SVD.Completion
import qualified EmHell.SVD.Query
import qualified EmHell.SVD.Manipulation
import qualified EmHell.SVD.Selector
import qualified HOCD
import qualified Prettyprinter
import qualified Prettyprinter.Render.Terminal
import qualified System.Console.Repline

import Options.Applicative

type Repl a =
  HaskelineT
    (ReaderT Device
      (OCDT IO)
    ) a

main :: IO ()
main = do
  opts <- runOpts

  liftIO
    $ putStrLn
    $ "Loading SVD file " <> optsSVD opts

  x <- Data.SVD.IO.parseSVD
         (optsSVD opts)
  dev <- case x of
    Left err -> error err
    Right dev -> pure dev

  let ocdConfig =
        def
          { ocdHost =
              Data.Maybe.fromMaybe
                (ocdHost def)
                (optsHost opts)
          , ocdPort =
              Data.Maybe.fromMaybe
                (ocdPort def)
                (optsPort opts)
          }

  _ <- HOCD.runOCDConfig ocdConfig $ do
    Control.Monad.void
      $ (`runReaderT` dev)
      $ runRepl
  pure ()

runRepl :: ReaderT Device (OCDT IO) ()
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
          SingleLine -> "hocd> "
          MultiLine -> "| "

    options :: [(String, String -> Repl ())]
    options = [
        ("set", setReg)
      ]

    completion :: CompleterStyle (ReaderT Device (OCDT IO))
    completion =
       Prefix
        ( EmHell.SVD.Completion.compFunc
          $ (ask >>=)
          . flip EmHell.SVD.Completion.svdCompleter
        )
        defaultMatcher

    defaultMatcher :: [(String, CompletionFunc (ReaderT Device (OCDT IO)))]
    defaultMatcher =
      [ ( ":set"
        , EmHell.SVD.Completion.compFunc
            $ (ask >>=)
            . flip EmHell.SVD.Completion.svdCompleterFields
        )
      ]

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
          Right (reg, regAddr) -> do
            regVal <- lift
              . HOCD.readMem32
              . HOCD.memAddr
              $ fromIntegral regAddr
            liftIO
              $ Data.SVD.Pretty.Explore.exploreRegister
                  regVal
                  regAddr
                  reg

          Left e -> liftIO $ putStrLn e

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
            let regMemAddr =
                  HOCD.memAddr
                  $ fromIntegral regAddr

            origRegVal <-
              lift $ HOCD.readMem32 regMemAddr

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
                  $ HOCD.writeMem32
                      regMemAddr
                      [newVal]

                newRegVal <- lift $ HOCD.readMem32 regMemAddr

                liftIO
                  $ Data.SVD.Pretty.Explore.exploreRegister
                      newRegVal
                      regAddr
                      reg

          Left e -> liftIO $ putStrLn e

data Options = Options
  { optsSVD :: FilePath
  , optsHost :: Maybe String
  , optsPort :: Maybe Int
  } deriving Show

parseOptions :: Parser Options
parseOptions = Options <$>
      EmHell.Options.parseSVD
  <*> optional (strOption $
          long "host"
       <> metavar "HOST"
       <> help "Host where OpenOCD is listening")
  <*> optional
        (
          read
          <$> strOption
            (  long "port"
            <> help "OpenOCD RPC port"
            <> showDefault
            <> value "6666"
            )
        )

runOpts :: IO Options
runOpts =
  execParser
  $ info
      (parseOptions <**> helper)
      (fullDesc <> progDesc "emhell")
