module EmHell.SVD.Completion
  ( svdCompleter
  , svdCompleter'
  , svdCompleterMay
  , compFunc
  ) where

import Control.Applicative (optional)
import Control.Monad.Trans.State.Strict (StateT, get)

import Data.Attoparsec.Text (Parser)
import Data.SVD
  ( Device(..)
  , Field(..)
  , Peripheral(..)
  , Register(..)
  )
import Data.Text (Text)
import System.Console.Haskeline.Completion
  ( Completion(..)
  , CompletionFunc
  )

import qualified Data.Attoparsec.Text
import qualified Data.Char
import qualified Data.Either
import qualified Data.List
import qualified Data.Maybe
import qualified Data.SVD
import qualified Data.Text
import qualified System.Console.Haskeline.Completion

parsePart :: Parser Text
parsePart =
  Data.Attoparsec.Text.takeWhile1 (/='.')
  <* (optional $ Data.Attoparsec.Text.char '.')

svdCompleterMay
  :: Monad m
  => String
  -> StateT (Maybe Device) m [String]
svdCompleterMay x = do
  s <- get
  case s of
    Nothing -> pure mempty
    Just dev -> svdCompleter dev x

compFunc
  :: Monad m
  => (String -> m [String])
  -> CompletionFunc m
compFunc f =
  System.Console.Haskeline.Completion.completeWord
    (Just '\\')
    " \t()[]"
    $ \x ->
        map
          ( notFinished
          . System.Console.Haskeline.Completion.simpleCompletion
          )
          <$> f x
  where
    notFinished :: Completion -> Completion
    notFinished x = x { isFinished = False }

svdCompleter'
  :: Monad m
  => Bool -- ^ Complete fields
  -> Device
  -> String
  -> m [String]
svdCompleter' completeFields dev x =
  nestedCompleter
    (map
      (periphName)
      $ devicePeripherals dev
    ) x
    $ \(complete, leftover) -> do
      let f = Data.Either.fromRight mempty
              $ Data.SVD.getPeriphRegs complete dev
      nestedCompleter
        (map regName f)
        leftover
        $ if not completeFields
          then \_ -> pure mempty
          else
            \(complete2, leftover2) -> do
              let f' = Data.Either.fromRight mempty
                       $ Data.SVD.getPeriphRegFields
                           complete
                           complete2
                           dev
              nestedCompleter
                (map fieldName f')
                leftover2
                $ \_ -> pure mempty

svdCompleter
  :: Monad m
  => Device
  -> String
  -> m [String]
svdCompleter = svdCompleter' False

nestedCompleter
  :: Monad m
  => [String]
  -> String
  -> ((String, String) -> m [String])
  -> m [String]
nestedCompleter names input nest =
  nestedCompleter'
    (\x ->
          (Data.List.isPrefixOf x)
        . map Data.Char.toLower)
    (map
      (map Data.Char.toLower) names)
      input
    nest

nestedCompleter'
  :: Monad m
  => (String -> String -> Bool)
  -> [String]
  -> String
  -> ((String, String) -> m [String])
  -> m [String]
nestedCompleter' matchFn names input nest = do
  let xinput =
        if input == "."
        then ""
        else if "." `Data.List.isPrefixOf` input
             then drop 1 input
             else input
  -- ouch
  --let x = trace (show ("zz", xinput)) (return ())
  --x
  case
      Data.Text.unpack
      <$> Data.Attoparsec.Text.parseOnly
            parsePart
            (Data.Text.pack xinput)
    of
    Left _e -> pure names

    Right x | complete x -> do
      res <-
        nest
          ( x
          , Data.Maybe.fromJust
            $ x `Data.List.stripPrefix` xinput)
      pure $ map (prefix x) res

    Right x | otherwise ->
      pure $ filter (matchFn x) names

  where
    complete x =
        not
      . null
      $ filter (==x) names

    prefix with x =
      concat
        [ with
        , "."
        , x
        ]
