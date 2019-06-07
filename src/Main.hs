module Main (main) where

import Control.Applicative ((<|>))

import qualified Options.Applicative as Options

import           Data.Foldable    (traverse_)
import           Data.Monoid      ((<>))
import qualified Data.Text        as Text
import           Data.Text.Indent (Options (..), defaultOptions, fixIndentation)
import qualified Data.Text.IO     as Text

-- | Command-line options parser
optionsInfo :: Options.ParserInfo Options
optionsInfo =
  Options.info (Options.helper <*> (spaces <|> pure defaultOptions)) mempty
  where
    spaces = Options ' ' <$>
      Options.option
        Options.auto
        (Options.short 's' <> Options.long "spaces" <> Options.metavar "NUM")

-- | Indentation
main :: IO ()
main = do
  options <- Options.execParser optionsInfo
  content <- Text.getContents
  traverse_ Text.putStrLn (fixIndentation options (Text.lines content))
