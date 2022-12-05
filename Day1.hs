module Main where

import System.IO
import Data.List
import Data.Ord

main = do 
    part1
    part2

withFile' :: String -> (String -> IO ()) -> IO ()
withFile' path f = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    result <- f contents
    hClose handle
    return result

-- Day 1 --
elvesTotalCalories :: String -> [Int]
elvesTotalCalories s = map (sum) . (map . map) (read :: String -> Int) $ split $ lines s

maxCalories :: String -> IO ()
maxCalories s = print $ maximum $ elvesTotalCalories s

part1 = withFile' "day1.txt" maxCalories

top3Calories :: String -> IO ()
top3Calories s = print $ sum $ take 3 (sortBy (comparing Down) (elvesTotalCalories s))

part2 = withFile' "day1.txt" top3Calories

split :: [String] -> [[String]]
split s = case dropWhile (== "") s of
    [] -> []
    s' -> w : split s''
        where (w, s'') = break (== "") s'

-- Day 2 --



