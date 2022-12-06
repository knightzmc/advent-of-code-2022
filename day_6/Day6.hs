import Data.List ( nub )

windowed :: Int -> [a] -> [[a]]
windowed n [] = []
windowed n xs | length xs < 4 = [xs]
windowed n xs = take n xs : windowed n (tail xs)

isMarker :: Eq a => [a] -> Bool
isMarker xs = nub xs == xs

main :: IO ()
main = do
    c <- readFile "input.txt"
    print $ (4 +) $ length $ takeWhile (not . isMarker) (windowed 4 c)
    print $ (14 +) $ length $ takeWhile (not . isMarker) (windowed 14 c)