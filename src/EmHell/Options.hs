module EmHell.Options
  ( parseSVD
  ) where

import Options.Applicative

parseSVD :: Parser FilePath
parseSVD =
  strOption
    $  long "svd"
    <> metavar "XML"
    <> help "Load software vendor description XML file"
