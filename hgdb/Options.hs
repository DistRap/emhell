module Options where

import Gdb (Programmer(..))
import Options.Applicative
import qualified EmHell.Parsers

data Options = Options {
    optsProg  :: Maybe Programmer
  , optsFile  :: Maybe FilePath
  , optsMILog :: Maybe FilePath
  , optsSVD   :: Maybe FilePath
  , optsCwd   :: Maybe FilePath
  , optsArm   :: Bool
  , optsEx    :: [String]
  } deriving (Show)

parseProgrammer :: Parser Programmer
parseProgrammer =
   (BMP <$> strOption
      (  long "bmp"
      <> metavar "DEV"
      <> help "Use BlackMagic Probe at DEV"
      )
   )
 <|>
   (uncurry BMPHosted <$> option (EmHell.Parsers.hostPort)
      (  long "bmphosted"
      <> metavar "HOST:PORT"
      <> help "Use hosted BlackMagic Probe at HOST:PORT"
      )
   )
 <|>
   (uncurry RemoteGDB <$> option (EmHell.Parsers.hostPort)
      (  long "remotegdb"
      <> metavar "HOST:PORT"
      <> help "Use remote GDB server at HOST:PORT"
      )
   )

parseOptions :: Parser Options
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

runOpts :: IO Options
runOpts =
  execParser
  $ info
      (parseOptions <**> helper)
      (fullDesc <> progDesc "hgdb")
