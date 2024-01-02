{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module EmHell.SVD.Selector
  ( Selector(..)
  , parseSelector
  , parseSelectorValue
  ) where

import Data.Bits (Bits)
import Control.Applicative
import Data.Attoparsec.Text
import qualified Data.Text
import qualified Data.Char

data Selector a = Selector {
    selPeriph :: a
  , selReg    :: a
  , selField  :: Maybe a
  }
  deriving (Eq, Ord, Show, Functor)

nameParser :: Parser String
nameParser = many1 (letter <|> digit <> char '_')

-- | Parse Periph.Reg.Field selector
selectorParser :: Parser (Selector String)
selectorParser = do
  p <- nameParser
  _ <- char '.'
  r <- nameParser
  f <- optional (char '.' *> nameParser)
  pure
    $ map Data.Char.toLower
    <$> Selector p r f

selectorValueParser
  :: ( Integral a
     , Bits a
     )
  => Parser (Selector String, a)
selectorValueParser = do
  s <- selectorParser
  skipSpace
  _ <- char '='
  skipSpace
  v <- optional "0x" *> hexadecimal
  pure (s, v)

parseSelector
  :: String
  -> Either String (Selector String)
parseSelector =
  parseOnly selectorParser
  . Data.Text.pack

parseSelectorValue
  :: ( Integral a
     , Bits a
     )
  => String
  -> Either String (Selector String, a)
parseSelectorValue =
  parseOnly selectorValueParser
  . Data.Text.pack
