module EmHell.Parsers
  ( hostPort
  ) where

import Data.Attoparsec.Text (Parser)
import Options.Applicative (ReadM)
import qualified Data.Attoparsec.Text
import qualified Data.Text
import qualified Options.Applicative

attoReadM :: Parser a -> ReadM a
attoReadM p =
  Options.Applicative.eitherReader
    $ Data.Attoparsec.Text.parseOnly p
    . Data.Text.pack

parseAddress :: Parser String
parseAddress =
  Data.Text.unpack
  <$> Data.Attoparsec.Text.takeWhile (/=':')

parsePort :: Parser Int
parsePort = do
  _ <- Data.Attoparsec.Text.char ':'
  d <- Data.Attoparsec.Text.decimal
  return d

hostPortP :: Parser (String, Int)
hostPortP = (,) <$> parseAddress <*> parsePort

hostPort :: ReadM (String, Int)
hostPort = attoReadM hostPortP
