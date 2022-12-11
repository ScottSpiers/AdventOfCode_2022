module Main where

import System.IO
import Data.List
import Data.Ord

main = do 
    part1
    part2

withFile' :: (Show a) => String -> (String -> a) -> IO ()
withFile' path f = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    print $ f contents
    hClose handle

split :: [String] -> [[String]]
split s = case dropWhile (== "") s of
    [] -> []
    s' -> w : split s''
        where (w, s'') = break (== "") s'

mapRead :: [[String]] -> [[Int]]
mapRead s =  (map . map) (read :: String -> Int) s

-- Day 1 --
elvesTotalCalories :: String -> [Int]
elvesTotalCalories s = map (sum) . mapRead . split $ lines s

maxCalories :: String -> Int
maxCalories s = maximum $ elvesTotalCalories s

part1 = withFile' "day1.txt" maxCalories

top3Calories :: String -> Int
top3Calories s = (sum . take 3 . sortBy (comparing Down)) $ elvesTotalCalories s

part2 = withFile' "day1.txt" top3Calories


-- Day 2 --



