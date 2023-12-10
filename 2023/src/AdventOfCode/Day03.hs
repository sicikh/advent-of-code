module AdventOfCode.Day03 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude
import Data.ByteString qualified as BS
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet qualified as HashSet

solution :: Solution
solution =
    Solution
        { parser = parseScheme
        , part1 = sum . partNumbers
        , part2 = sum . gearRatios
        }

data Part = Number Int | Symbol Char
    deriving (Show)

type Pos = (Int, Int)

parseLine :: Int -> Parser [(Int, Int, Part)]
parseLine !offset = do
    (n, _) <- withOffset (Parser.takeWhile (== '.'))
    (l, part) <- withOffset (number <|> symbol)
    let p = (offset + n, l, part)
    ps <- parseLine (offset + n + l) <|> pure []
    Parser.skipWhile (== '.')
    pure (p : ps)
    where
        number = Number <$> Parser.decimal
        symbol = Symbol <$> Parser.satisfy (Parser.notInClass ".\n\r")
        withOffset p = first BS.length <$> Parser.match p

parseScheme :: Parser [(Pos, Int, Part)]
parseScheme = enumerate <$> parseLine 0 `sepEndBy` Parser.endOfLine
    where
        enumerate parts = do
            (y, row) <- zip [0 ..] parts
            (x, l, part) <- row
            pure ((x, y), l, part)

adjacents :: (Int, Int) -> Int -> [(Int, Int)]
adjacents (x, y) l = do
    dx <- [-1 .. l]
    dy <- [-1 .. 1]
    pure (x + dx, y + dy)

partNumbers :: [(Pos, Int, Part)] -> [Int]
partNumbers parts = do
    ((x, y), l, Number n) <- parts
    guard $ any (`HashSet.member` symbols) $ adjacents (x, y) l
    pure n
    where
        symbols = HashSet.fromList [pos | (pos, _, Symbol _) <- parts]

gearRatios :: [(Pos, Int, Part)] -> [Int]
gearRatios parts = do
    ((x, y), _, Symbol '*') <- parts
    let adj = do
            dx <- [-3 .. 1]
            dy <- [-1 .. 1]
            case HashMap.lookup (x + dx, y + dy) numbers of
                Just (l, n) | l >= -dx -> pure n
                _ -> []
    case adj of
        [n1, n2] -> pure (n1 * n2)
        _ -> []
    where
        numbers = HashMap.fromList $ [(pos, (l, n)) | (pos, l, Number n) <- parts]
