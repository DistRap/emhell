module Options where

import Gdb

import Options.Applicative
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B

attoReadM :: A.Parser a -> ReadM a
attoReadM p = eitherReader (A.parseOnly p . B.pack)

data Options = Options {
    optsProg  :: Maybe Programmer
  , optsFile  :: Maybe FilePath
  , optsMILog :: Maybe FilePath
  , optsSVD   :: Maybe FilePath
  , optsCwd   :: Maybe FilePath
  , optsArm   :: Bool
  , optsEx    :: [String]
  } deriving (Show)

parseAddress :: A.Parser String
parseAddress = B.unpack <$> A.takeWhile (/=':')

parsePort :: A.Parser Int
parsePort = do
  _ <- A.char ':'
  d <- A.decimal
  return d

hostPort = (,) <$> parseAddress <*> parsePort

parseProgrammer =
   (BMP <$> strOption (
         long "bmp"
      <> metavar "DEV"
      <> help "Use BlackMagic Probe at DEV"))
 <|>
   (uncurry BMPHosted <$> option (attoReadM hostPort) (
         long "bmphosted"
      <> metavar "HOST:PORT"
      <> help "Use hosted BlackMagic Probe at HOST:PORT"))

parseOptions = Options <$>
      optional parseProgrammer
  <*> optional (strOption $
          long "file"
       <> metavar "FILE"
       <> help "Load FILE to Gdb")
  <*> optional (strOption $
          long "milog"
       <> metavar "FILE"
       <> help "Log Gdb/MI session to FILE")
  <*> optional (strOption $
          long "svd"
       <> metavar "XML"
       <> help "Load software vendor description XML file")
  <*> optional (strOption $
          long "cwd"
       <> metavar "DIR"
       <> help "Change directory prior any operations")
  <*> switch (
          long "arm"
       <> short 'a'
       <> help "Use arm-none-eabi-gdb")
  <*> many (strOption $
          long "ex"
       <> short 'e'
       <> help "Execute")

runOpts = execParser $ info (parseOptions <**> helper) (fullDesc <> progDesc "hgdb")
