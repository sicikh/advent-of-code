module AdventOfCode.Day02 (solution) where

import AdventOfCode.Parser qualified as Parser
import AdventOfCode.Prelude

solution :: Solution
solution =
    Solution
        { parser = parseGame `sepEndBy'` Parser.endOfLine
        , part1 = sumValidGameIds
        , part2 = powerCubes
        }

data Draw = Draw {red :: Int, green :: Int, blue :: Int}

data Game = Game {gid :: Int, draw :: Draw}

instance Semigroup Draw where
    (<>) :: Draw -> Draw -> Draw
    (Draw q w e) <> (Draw z x c) = Draw (max q z) (max w x) (max e c)

instance Monoid Draw where
    mempty :: Draw
    mempty = Draw 0 0 0

isValid :: Draw -> Bool
isValid (Draw r g b) = r <= 12 && g <= 13 && b <= 14

validGames :: [Game] -> [Game]
validGames = filter (isValid . draw)

sumValidGameIds :: [Game] -> Int
sumValidGameIds = sum . map gid . validGames

powerCubes :: [Game] -> Int
powerCubes = sum . map (power . draw)
    where
        power (Draw r g b) = r * g * b

parseGame :: Parser Game
parseGame = Game <$> (Parser.symbol "Game " *> Parser.decimal <* Parser.char ':') <*> draw
    where
        draw = mconcat <$> cube `Parser.sepBy1'` Parser.char ';'
        cube = mconcat <$> color `Parser.sepBy1'` Parser.char ','
        color =
            Parser.skipSpace *> Parser.decimal <* Parser.skipSpace >>= \i ->
                Draw i 0 0
                    <$ Parser.symbol "red"
                        <|> Draw 0 i 0
                    <$ Parser.symbol "green"
                        <|> Draw 0 0 i
                    <$ Parser.symbol "blue"
