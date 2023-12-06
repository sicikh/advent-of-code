module AdventOfCode.Day01 (solution) where

import qualified AdventOfCode.Parser as Parser
import AdventOfCode.Prelude
import qualified Data.ByteString.Char8 as BS
import Data.Char (isAlphaNum, isDigit, ord)
import qualified Data.Trie as Trie

solution :: Solution
solution =
    Solution
        { parser = Parser.takeWhile1 isAlphaNum `sepEndBy` Parser.endOfLine
        , part1 = solve parseNumDigits
        , part2 = solve parseSpelledDigit
        }

solve :: (ByteString -> [Int]) -> [ByteString] -> Int
solve parseDigits = sum . map (joinExtremeDigits . parseDigits)
    where
        joinExtremeDigits xs = head xs * 10 + last xs

parseNumDigits :: ByteString -> [Int]
parseNumDigits = BS.foldr (\c ds -> if isDigit c then (ord c - ord '0') : ds else ds) []

parseSpelledDigit :: ByteString -> [Int]
parseSpelledDigit = go trie
    where
        go tr bs =
            case tr `Trie.match` bs of
                Nothing | BS.null bs -> []
                Nothing -> go tr (BS.drop 1 bs)
                Just (mbs, i, bss) ->
                    i : go tr ((if isDigit $ BS.head mbs then mempty else BS.drop (BS.length mbs - 1) mbs) <> bss)
        trie =
            Trie.fromList $
                zip ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"] [1 ..]
                    <> zip ["1", "2", "3", "4", "5", "6", "7", "8", "9"] [1 ..]
