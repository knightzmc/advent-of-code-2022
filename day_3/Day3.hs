import Data.Char
import Data.List
import Data.List.Split

priority :: Char -> Int
priority c =
    if isUpper c
        then ord c - ord 'A' + 27
        else ord c - ord 'a' + 1

type Rucksack = (String, String)

parseRucksack :: String -> Rucksack
parseRucksack s = let middle = length s `div` 2 in splitAt middle s

common :: Rucksack -> String
common = nub . uncurry intersect

findBadge :: [String] -> Char
findBadge strings = head $ nub (foldr1 intersect strings)

main :: IO ()
main = do
    c <- lines <$> readFile "input.txt"
    let rucksacks = parseRucksack <$> c
    let commons = common =<< rucksacks
    print (sum (priority <$> commons))

    -- part 2
    let groups = chunksOf 3 c
    let badges = findBadge <$> groups
    print (sum (priority <$> badges))
