module Main (main) where

import Control.Applicative ((<|>))

import qualified Options.Applicative as Options

import           Data.Foldable    (traverse_)
import           Data.Maybe       (fromMaybe)
import           Data.Monoid      ((<>))
import qualified Data.Text        as Text
import           Data.Text.Indent (Options (..), defaultOptions, fixIndentation, guessOptions)
import qualified Data.Text.IO     as Text

-- | Command-line options parser
optionsInfo :: Options.ParserInfo (Maybe Options)
optionsInfo =
  Options.info (Options.helper <*> (spaces <|> pure Nothing)) mempty
  where
    spaces = Just . Options ' ' <$>
      Options.option
        Options.auto
        (Options.short 's' <> Options.long "spaces" <> Options.metavar "NUM")

-- | Indentation
main :: IO ()
main = do
  mbOptions <- Options.execParser optionsInfo

  content <- Text.getContents
  let lines = Text.lines content

  traverse_ Text.putStrLn $
    fixIndentation (fromMaybe defaultOptions (mbOptions <|> guessOptions lines)) lines
