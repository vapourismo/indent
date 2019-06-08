module Data.Text.Indent (Options (..), defaultOptions, fixIndentation) where

import           Data.Char (isSpace)
import qualified Data.Text as Text

-- | Line details
data Line = Line
  { linePrefixLength :: !Int       -- ^ Prefix length of the line (i.e. how many spaces/tabs)
  , lineBody         :: !Text.Text -- ^ Line body without prefix
  }

-- | Create a 'Line' from a full line.
toLine :: Char -> Text.Text -> Line
toLine character line = Line
  { linePrefixLength = Text.length prefix
  , lineBody         = body
  }
  where
    (prefix, body) = Text.span (== character) line

-- | Indentation block information
data Block = Block
  { blockPrefixLength :: !Int -- ^ Prefix length of the block (i.e. how many spaces/tabs)
  , blockLevel        :: !Int -- ^ Normalized indentation level
  }

-- | Find the current block. If no blocks are available default to the initial one.
findBlock :: [Block] -> Block
findBlock []          = Block 0 0
findBlock (block : _) = block

-- | Indentation options
data Options = Options
  { optionCharacter  :: !Char -- ^ Indentation character
  , optionMultiplier :: !Int  -- ^ Indentation multiplier
  }
  deriving (Show, Eq)

-- | Default indentation options
defaultOptions :: Options
defaultOptions = Options ' ' 2

-- | Indent lines
fixIndentation :: Options -> [Text.Text] -> [Text.Text]
fixIndentation (Options character multiplier) =
  run [] . map (toLine character)
  where
    mkPrefix level = Text.replicate (level * multiplier) (Text.singleton character)

    isEmptyLine = Text.all isSpace . lineBody

    fix blocks line =
      case findBlock blocks of
        Block prevPrefixLength prevLevel
          -- This line has a longer prefix than the previous block, indicating a new indentation
          -- block has started.
          | linePrefixLength line > prevPrefixLength
          , newLevel <- prevLevel + 1 ->
            ( Block {blockPrefixLength = linePrefixLength line, blockLevel = newLevel} : blocks
            , Text.append (mkPrefix newLevel) (lineBody line)
            )

          -- The current line has a shorted prefix than the previous block, meaning that the
          -- indentation block is done.
          | linePrefixLength line < prevPrefixLength ->
            fix (dropWhile (\block -> linePrefixLength line < blockPrefixLength block) blocks) line

          -- This line prefix is exactly as long as the current block's.
          | otherwise ->
            ( blocks
            , Text.append (mkPrefix prevLevel) (lineBody line)
            )

    run blocks lines =
      case lines of
        line : lines
          | isEmptyLine line                    -> Text.empty : run blocks lines
          | (blocks', line') <- fix blocks line -> line' : run blocks' lines

        [] -> []
