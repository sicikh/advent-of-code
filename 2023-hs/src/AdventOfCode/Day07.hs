module AdventOfCode.Day07 where

import qualified AdventOfCode.Parser as Parser
import AdventOfCode.Prelude
import Data.List (group, sort)

solution :: Solution
solution =
    Solution
        { parser = parseCards `sepEndBy'` Parser.endOfLine
        , part1 = solve hand
        , part2 = \_ -> 42 :: Int
        }

type Cards = [Card]

data Card
    = Two
    | Three
    | Four
    | Five
    | Six
    | Seven
    | Eight
    | Nine
    | Ten
    | Jack
    | Queen
    | King
    | Ace
    deriving (Bounded, Eq, Ord)

data Kind
    = HighCard
    | OnePair
    | TwoPair
    | ThreeKind
    | FullHouse
    | FourKind
    | FiveKind
    deriving (Show, Bounded, Eq, Ord)

data Hand = Hand
    { kind :: Kind
    , value :: Int
    }
    deriving (Show, Eq, Ord)

hand :: Cards -> Hand
hand cards =
    Hand
        { kind = makeKind cards
        , value = cardsValue cards
        }

cardValue :: Card -> Int
cardValue Ace = 14
cardValue King = 13
cardValue Queen = 12
cardValue Jack = 11
cardValue Ten = 10
cardValue Nine = 9
cardValue Eight = 8
cardValue Seven = 7
cardValue Six = 6
cardValue Five = 5
cardValue Four = 4
cardValue Three = 3
cardValue Two = 2

solve :: (Cards -> Hand) -> [(Cards, Int)] -> Int
solve makeHand = sum . zipWith (*) [1 ..] . map snd . sortOn (makeHand . fst)

cardsValue :: Cards -> Int
cardsValue = foldl' (\acc c -> cardValue c + 15 * acc) 0

makeKind :: Cards -> Kind
makeKind cards =
    case cardKinds cards of
        [] -> error "Invalid hand"
        (c : cs) -> case (c, cs) of
            (5, []) -> FiveKind
            (4, [1]) -> FourKind
            (3, [2]) -> FullHouse
            (3, [1, 1]) -> ThreeKind
            (2, [2, 1]) -> TwoPair
            (2, [1, 1, 1]) -> OnePair
            (1, [1, 1, 1, 1]) -> HighCard
            _ -> error "Invalid hand"
    where
        cardKinds = sortDesc . map length . group . sort
        sortDesc = sortBy (flip compare)

instance Show Card where
    show :: Card -> String
    show Ace = "A"
    show King = "K"
    show Queen = "Q"
    show Jack = "J"
    show Ten = "T"
    show Nine = "9"
    show Eight = "8"
    show Seven = "7"
    show Six = "6"
    show Five = "5"
    show Four = "4"
    show Three = "3"
    show Two = "2"

parseCards :: Parser (Cards, Int)
parseCards = do
    cards <- Parser.many1' parseCard
    Parser.whitespace
    bid <- Parser.decimal
    pure (cards, bid)

parseCard :: Parser Card
parseCard =
    (Ace <$ Parser.char 'A')
        <|> (King <$ Parser.char 'K')
        <|> (Queen <$ Parser.char 'Q')
        <|> (Jack <$ Parser.char 'Q')
        <|> (Ten <$ Parser.char 'T')
        <|> (Nine <$ Parser.char '9')
        <|> (Eight <$ Parser.char '8')
        <|> (Seven <$ Parser.char '7')
        <|> (Six <$ Parser.char '6')
        <|> (Five <$ Parser.char '5')
        <|> (Four <$ Parser.char '4')
        <|> (Three <$ Parser.char '3')
        <|> (Two <$ Parser.char '2')
