module AdventOfCode.Day04 (solution) where

import AdventOfCode.Prelude
import AdventOfCode.Parser qualified as Parser
import Data.HashSet qualified as HashSet

solution :: Solution
solution =
    Solution
        { parser = parseCard `sepEndBy'` Parser.endOfLine
        , part1 = scoreSum
        , part2 = copyScoreSum
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

scoreSum :: [Int] -> Int
scoreSum = sum . map score
    where
        score 0 = 0
        score m = 2 ^ (m - 1)

copyScoreSum :: [Int] -> Int
copyScoreSum = sum . foldr go []
    where
        go c cs = 1 + sum (take c cs) : cs
