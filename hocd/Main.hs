{-# LANGUAGE LambdaCase #-}

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
  , ExitDecision(Exit)
  )

import qualified Control.Monad
import qualified Data.Maybe
import qualified Data.SVD.IO
import qualified Data.SVD.Pretty.Explore
import qualified EmHell.Options
import qualified EmHell.SVD.Completion
import qualified EmHell.SVD.Query
import qualified EmHell.SVD.Selector
import qualified HOCD
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
    mempty
    (Just ':')
    (Just "paste")
    (Prefix
      ( EmHell.SVD.Completion.compFunc
        $ (ask >>=)
        . flip EmHell.SVD.Completion.svdCompleter
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
