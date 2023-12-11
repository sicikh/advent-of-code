module AdventOfCode.Day05 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude hiding (Map)
import Data.Char (isAsciiLower)

solution :: Solution
solution =
    Solution
        { parser = parseAlmanac
        , part1 = uncurry (findMinimalLocation . seeds)
        , part2 = uncurry (findMinimalLocation . seedRanges)
        }

-- | Exclusive from end range (like start..=end in Rust).
data Range = Range {start :: Int, end :: Int}
    deriving (Eq, Show)

-- | A list of rules for mapping.
type Map = [Rule]

{- | Rule for mapping one range into another.

    Contains source range and shift, which when applied to source gives destination range.
-}
type Rule = (Range, Int)

-- | Apply rules of single map to a range, optionally splitting it like a sieve.
applyMap :: Map -> Range -> [Range]
applyMap = foldr applyRule filterEmpty
    where
        filterEmpty range = [range | not (isEmpty range)]
        applyRule :: Rule -> (Range -> [Range]) -> Range -> [Range]
        applyRule (ruleRange, ruleShift) filterEmpty' sourceRange
            | isEmpty sourceRange = [] -- if source is empty, destination is empty too
            | isEmpty intersection = filterEmpty' sourceRange -- if mapping does not set rule, source is untouched
            | otherwise = intersection `shift` ruleShift : filterEmpty' left <> filterEmpty' right -- apply rule and optionally split source
            where
                intersection = sourceRange `intersect` ruleRange
                (left, right) = sourceRange `diff` ruleRange

findMinimalLocation :: [Range] -> [Map] -> Int
findMinimalLocation ranges =
    minimum
        . map start
        . foldl' (\rs m -> concatMap (applyMap m) rs) ranges

-- | Interpret a pair of seeds as two different seed numbers (or, exactly, as two ranges with one entry).
seeds :: [(Int, Int)] -> [Range]
seeds = concatMap (\(x, y) -> [Range x (x + 1), Range y (y + 1)])

-- | Interpret a pair of seeds as one range, that is set by start and length.
seedRanges :: [(Int, Int)] -> [Range]
seedRanges = map (\(x, y) -> Range x (x + y))

parseAlmanac :: Parser ([(Int, Int)], [Map])
parseAlmanac = do
    seedPairs <- Parser.symbol "seeds:" *> pair `sepBy` Parser.whitespace
    Parser.skipSpace
    maps <- parseMap `sepEndBy` Parser.endOfLine
    pure (seedPairs, maps)
    where
        pair = do
            x <- Parser.decimal <* Parser.space
            y <- Parser.decimal
            pure (x, y)

parseMap :: Parser Map
parseMap = do
    _name <- Parser.takeWhile1 (\c -> isAsciiLower c || c == '-') <* Parser.space
    Parser.string "map:" >> Parser.endOfLine
    parseRule `sepEndBy'` Parser.endOfLine

parseRule :: Parser Rule
parseRule = do
    destination <- Parser.decimal <* Parser.space
    source <- Parser.decimal <* Parser.space
    len <- Parser.decimal
    let ruleRange = Range source (source + len)
        ruleShift = destination - source
    pure (ruleRange, ruleShift)

isEmpty :: Range -> Bool
isEmpty (Range start end) = start >= end

shift :: Range -> Int -> Range
shift (Range start end) dx = Range (start + dx) (end + dx)

intersect :: Range -> Range -> Range
intersect (Range as ae) (Range bs be) = Range (max as bs) (min ae be)

diff :: Range -> Range -> (Range, Range)
diff (Range as ae) (Range bs be) =
    ( Range (min as bs) (min ae bs)
    , Range (max as be) (max ae be)
    )
