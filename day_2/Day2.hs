data Choice
    = Rock
    | Paper
    | Scissors
    deriving (Eq)

choiceValue :: Choice -> Int
choiceValue Rock = 1
choiceValue Paper = 2
choiceValue Scissors = 3

data Result
    = Win
    | Lose
    | Draw

resultValue :: Result -> Int
resultValue Win = 6
resultValue Draw = 3
resultValue Lose = 0

play :: Choice -> Choice -> Int
play x y = let result = play' x y in (resultValue result + choiceValue x)
  where
    play' x y | x == y = Draw
    play' Rock Scissors = Win
    play' Rock Paper = Lose
    play' Scissors Paper = Win
    play' Scissors Rock = Lose
    play' Paper Rock = Win
    play' Paper Scissors = Lose

parse :: Char -> Choice
parse 'A' = Rock
parse 'X' = Rock
parse 'B' = Paper
parse 'Y' = Paper
parse 'C' = Scissors
parse 'Z' = Scissors

parseInput :: String -> (Choice, Choice)
parseInput (a : ' ' : b : xs) = (parse a, parse b)
parseInput other = error ("Invalid input " <> other)

-- part 2
parse2 :: Char -> Result
parse2 'X' = Lose
parse2 'Y' = Draw
parse2 'Z' = Win

parseInput2 :: String -> (Choice, Result)
parseInput2 (a : ' ' : b : xs) = (parse a, parse2 b)
parseInput2 other = error ("Invalid input " <> other)

play2 :: Choice -> Result -> Int
play2 x Draw = play x x
play2 Rock Win = play Paper Rock
play2 Rock Lose = play Scissors Rock
play2 Scissors Win = play Rock Scissors
play2 Scissors Lose = play Paper Scissors
play2 Paper Win = play Scissors Paper
play2 Paper Lose = play Rock Paper

main :: IO ()
main = do
    inputLines <- lines <$> readFile "input.txt"
    let parsed = parseInput <$> inputLines
    let res = sum (uncurry (flip play) <$> parsed)
    print res

    let parsed2 = parseInput2 <$> inputLines
    let res2 = sum (uncurry play2 <$> parsed2)
    print res2