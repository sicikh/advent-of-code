module AdventOfCode.Day04 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.HashSet qualified as HashSet

solution :: Solution
solution =
    Solution
        { parser = parseCard `sepEndBy'` Parser.endOfLine
        , part1 = sum . map score
        , part2 = sum . copyScore
        }

parseCard :: Parser Int
parseCard = do
    Parser.symbol "Card"
    _cid <- Parser.decimal @Int
    Parser.symbol ":"
    winning <- Parser.decimal @Int `sepEndBy'` Parser.takeWhile (== ' ')
    Parser.symbol "|"
    numbers <- Parser.decimal `sepEndBy'` Parser.takeWhile (== ' ')

    let winningSet = HashSet.fromList winning

    pure $ count (`HashSet.member` winningSet) numbers

score :: Int -> Int
score 0 = 0
score m = 2 ^ (m - 1)

copyScore :: [Int] -> [Int]
copyScore = foldr (\s ss -> 1 + sum (take s ss) : ss) []
