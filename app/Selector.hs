{-# LANGUAGE DeriveFunctor #-}
module Selector where

import Control.Applicative
import Data.Char (toLower)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

data Selector a = Selector {
    selPeriph :: a
  , selReg    :: a
  , selField  :: Maybe a
  }
  deriving (Eq, Ord, Show, Functor)

selectorParser :: Parser (Selector String)
selectorParser = do
  p <- takeWhile1 (/='.')
  _ <- char '.'
  r <- takeWhile1 (/='.')
  f <- optional (char '.' *> takeWhile1 (pure True) <* endOfInput)
  return $ map toLower . B.unpack <$> Selector p r f

parseSelector = parseOnly selectorParser . B.pack
