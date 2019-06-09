module Data.Text.Indent (Options (..), defaultOptions, guessOptions, fixIndentation) where

import           Data.Char       (isSpace)
import           Data.Function   (on)
import           Data.List       (groupBy, sortBy)
import qualified Data.Map.Strict as Map
import           Data.Maybe      (listToMaybe, mapMaybe)
import qualified Data.Set        as Set
import qualified Data.Text       as Text

----------------------------------------------------------------------------------------------------

-- | Indentation options
data Options = Options
  { optionCharacter  :: !Char -- ^ Indentation character
  , optionMultiplier :: !Int  -- ^ Indentation multiplier
  }
  deriving (Show, Eq)

-- | Default indentation options
defaultOptions :: Options
defaultOptions = Options ' ' 2

-- | List of possible multipliers.
possibleMultipliers :: Set.Set Int
possibleMultipliers = Set.fromList [1 .. 8]

-- | Guess a 'Options' that match the given lines.
guessOptions :: [Text.Text] -> Maybe Options
guessOptions =
  (>>= toOptions)
  -- Keep the multipliers that were used the most and those that were used at least 66% of the time
  -- of the maximum multiplier.
  . fmap (fmap keep66th)
  . listToMaybe
    -- Sort in a way that makes the character that was used the most the head of the list.
  . sortBy (on (flip compare) (maximum . snd))
  -- Group multipliers by character.
  . map gatherGrouped
  . groupBy (on (==) fst)
  . sortBy (on compare fst)
  -- We only want characters that are used for indentation.
  . filter (isSpace . fst)
  -- Guess indentation for all lines.
  . mapMaybe guessLineIndentation
  where
    gatherGrouped cs =
      ( fst (head cs)
      , foldr (Map.unionWith (+) . snd) (Map.fromSet (const 0) possibleMultipliers) cs
      )

    moreThan66th x y = y >= div (x * 2) 3

    keep66th vs = Map.filter (moreThan66th (maximum vs)) vs

    pickMultiplier (m, _    ) []                  = m
    pickMultiplier l@(m, times) (r@(m', times') : ms)
      | m' > m && moreThan66th times times' = pickMultiplier r ms
      | m' < m && moreThan66th times' times = pickMultiplier l ms
      | times' > times                      = pickMultiplier r ms
      | otherwise                           = pickMultiplier l ms

    toOptions (char, multipliers)
      | Map.null multipliers         = Nothing
      | m <- Map.findMax multipliers = Just Options
        { optionCharacter  = char
        , optionMultiplier = pickMultiplier m (Map.toList (Map.delete (fst m) multipliers))
        }

-- | Guess the character used for indentation and account for possible multipliers.
guessLineIndentation :: Text.Text -> Maybe (Char, Map.Map Int Int)
guessLineIndentation line
  | Text.null line || Text.all isSpace line = Nothing
  | otherwise                               = Just (initChar, lineMultipliers)
  where
    initChar = Text.head line

    prefixLength = Text.length (Text.takeWhile (initChar ==) line)

    lineMultipliers =
      Map.fromSet (\multiplier -> 1 - signum (mod prefixLength multiplier)) possibleMultipliers

----------------------------------------------------------------------------------------------------

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
