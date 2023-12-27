{-# LANGUAGE DeriveFunctor #-}
module EmHell.SVD.Selector where

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

-- | Parse Periph.Reg.Field selector
selectorParser :: Parser (Selector String)
selectorParser = do
  p <- takeWhile1 (/='.')
  _ <- char '.'
  r <- takeWhile1 (/='.')
  f <- optional (char '.' *> takeWhile1 (pure True) <* endOfInput)
  pure
    $ map Data.Char.toLower
    . Data.Text.unpack
    <$> Selector p r f

parseSelector
  :: String
  -> Either String (Selector String)
parseSelector =
  parseOnly selectorParser
  . Data.Text.pack
