{-# LANGUAGE OverloadedStrings #-}

import Data.List (sortOn)
import Data.Ord (Down (Down))
import Data.Text qualified as T
import Data.Text.IO qualified as T

type Data = [[Int]]

loadData :: IO Data
loadData = do
    contents <- T.readFile "data.txt"
    let groups = T.splitOn "\n" <$> T.splitOn "\n\n" contents
    pure $ (fmap . fmap) (read . T.unpack) groups

part1 :: Data -> Int
part1 nums = maximum (sum <$> nums)

part2 :: Data -> Int
part2 nums = do
    let sums = sum <$> nums
    let sorted = sortOn Down sums
    sum (take 3 sorted)

main :: IO ()
main = do
    nums <- loadData
    print (part1 nums)
    print (part2 nums)
