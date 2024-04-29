module AdventOfCode.Day06 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.Bifunctor (bimap)
import GHC.Float (floorDouble)

solution :: Solution
solution =
    Solution
        { parser = parseRaces
        , part1 = product . map (uncurry wins) . uncurry zip
        , part2 = uncurry wins . bimap removeSpaces removeSpaces
        }

wins :: Int -> Int -> Int
wins time distance = x1 - x2
    where
        b = fromIntegral time
        c = fromIntegral distance + 0.5
        d = b ^ (2 :: Int) - 4 * c
        s = sqrt d
        x1 = floorDouble $ (b + s) / 2
        x2 = floorDouble $ (b - s) / 2

removeSpaces :: [Int] -> Int
removeSpaces = read . concatMap show

parseRaces :: Parser ([Int], [Int])
parseRaces = do
    times <- parseTimes <* Parser.endOfLine
    distances <- parseDistances <* Parser.endOfLine
    pure (times, distances)
    where
        parseTimes = Parser.symbol "Time:" *> Parser.decimal `sepBy` Parser.whitespace
        parseDistances = Parser.symbol "Distance:" *> Parser.decimal `sepBy` Parser.whitespace
