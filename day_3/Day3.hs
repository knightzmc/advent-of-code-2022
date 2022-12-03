import Data.Char
import Data.List

priority :: Char -> Int
priority c =
    if isUpper c
        then ord c - ord 'A' + 27
        else ord c - ord 'a' + 1

type Rucksack = (String, String)

parseRucksack :: String -> Rucksack
parseRucksack s = let middle = length s `div` 2 in splitAt middle s

parseGroups :: [a] -> [(a, a, a)]
parseGroups [] = []
parseGroups xs =
    let ([a1, a2, a3], zs) = splitAt 3 xs
     in (a1, a2, a3) : parseGroups zs

common :: Rucksack -> String
common = nub . uncurry intersect

findBadge :: (String, String, String) -> Char
findBadge (a, b, c) = head $ nub (a `intersect` b `intersect` c)

main :: IO ()
main = do
    c <- lines <$> readFile "input.txt"
    let rucksacks = parseRucksack <$> c
    let commons = common =<< rucksacks
    print (sum (priority <$> commons))

    -- part 2
    let groups = parseGroups c
    let badges = findBadge <$> groups
    print (sum (priority <$> badges))
