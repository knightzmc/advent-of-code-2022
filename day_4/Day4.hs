import Control.Monad ( join )
import Data.Bifunctor (Bifunctor (bimap), second)
import Data.List ( elemIndex )
import Data.Maybe (fromJust)

data Range = Range Int Int

contains :: Pair -> Bool
contains (Range f1 l1, Range f2 l2) =
    (f1 <= f2 && l1 >= l2) || (f2 <= f1 && l2 >= l1)

overlaps :: Pair -> Bool
overlaps (Range f1 l1, Range f2 l2) = l1 >= f2 && f1 <= l2

type Pair = (Range, Range)

splitOn :: Char -> String -> (String, String)
splitOn c s = second tail $ splitAt (fromJust $ c `elemIndex` s) s

mapBoth :: Bifunctor p => (c -> d) -> p c c -> p d d
mapBoth = join bimap

parsePair :: String -> Pair
parsePair s =
    let parts = splitOn ',' s
     in mapBoth parseRange parts
  where
    parseRange s' = uncurry Range $ mapBoth read $ splitOn '-' s'

main :: IO ()
main = do
    c <- lines <$> readFile "input.txt"
    let pairs = parsePair <$> c
    print (length (filter contains pairs))
    print (length (filter overlaps pairs))





    