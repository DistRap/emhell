module Completion (
    svdCompleter
  , svdCompleterMay
  , compFunc
  ) where

import Control.Applicative
import Control.Monad.Trans.State.Strict

import Data.Char (toLower)
import Data.List (isPrefixOf, stripPrefix)
import Data.Either
import Data.Maybe
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as B

import Data.SVD.Types

import System.Console.Haskeline.Completion

parsePart :: Parser String
parsePart = B.unpack <$> takeWhile1 (/='.') <* (optional $ char '.')

svdCompleterMay :: Monad m => String -> StateT (Maybe Device) m [String]
svdCompleterMay x = do
  s <- get
  case s of
    Nothing -> return []
    Just dev -> svdCompleter dev x

compFunc f =  completeWord (Just '\\') " \t()[]" $ \x -> map (notFinished . simpleCompletion) <$> f x

-- mark completions as not finished
neverFinished :: (Monad m) => CompletionFunc m -> CompletionFunc m
neverFinished c (x, _y) = c (x, []) >>= return . (\(a,b) -> (a, map notFinished b))

notFinished x = x { isFinished = False }

svdCompleter :: Monad m => Device -> String -> m [String]
svdCompleter dev x = nestedCompleter (map (periphName) $ devicePeripherals dev) x $
  \(complete, leftover) -> do
    let f = (fromRight [] $ getPeriphRegs complete dev)
    nestedCompleter (map regName f) leftover $
      \(complete2, leftover2) -> do
        let f = (fromRight [] $ getPeriphRegFields complete complete2 dev)
        nestedCompleter (map fieldName f) leftover2 $ \_ ->
          return []

nestedCompleter names input nest = nestedCompleter'
  (\x -> (isPrefixOf x) . map toLower) (map (map toLower) names) input nest

nestedCompleter' matchFn names input nest = do
  let xinput = if input == "." then "" else if "." `isPrefixOf` input then drop 1 input else input
  -- ouch
  --let x = trace (show ("zz", xinput)) (return ())
  --x
  case parseOnly parsePart (B.pack xinput) of
    Left e -> return names
    Right x | complete x -> do
      res <- nest (x, (fromJust $ stripPrefix x xinput))
      return $ map (prefix x) res
    Right x | otherwise -> return $ filter (matchFn x) names
  where
    complete x = not . null $ filter (==x) names
    prefix with x = concat [with, ".", x]
