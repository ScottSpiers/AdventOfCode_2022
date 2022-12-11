module Main where

import System.IO
import Data.List
import Data.Ord

main = do 
    dayOnePartOne
    dayOnePartTwo
    dayTwoPartOne
    dayTwoPartTwo

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

dayOnePartOne = withFile' "day1.txt" maxCalories

top3Calories :: String -> Int
top3Calories s = (sum . take 3 . sortBy (comparing Down)) $ elvesTotalCalories s

dayOnePartTwo = withFile' "day1.txt" top3Calories


-- Day 2 --

data Choice = Rock | Paper | Scissors deriving (Bounded, Enum, Eq, Show)

data Result = Win | Draw | Lose deriving (Enum, Show)


pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp (x:[]) = []
pairUp (x:y:xs) = (x, y) : pairUp xs

evaluate :: (Choice, Choice) -> Int
evaluate (x, y) -- +1 as Enums start at 0
    | x == y = fromEnum y + 4
    | x == prev y = fromEnum y + 7
    | otherwise = fromEnum y + 1

next :: (Enum a, Bounded a, Eq a) => a -> a
next x 
        | x == maxBound = minBound
        | otherwise = succ x

prev :: (Enum a, Bounded a, Eq a) => a -> a
prev x 
        | x == minBound = maxBound
        | otherwise = pred x

assumedStrategyGuide :: String -> [(Choice, Choice)]
assumedStrategyGuide s = pairUp . map (decodeChoice) $ words s

calcStrategy :: (Enum a, Enum b) => ((a, b) -> Int) -> [(a, b)] -> Int
calcStrategy f xs = foldl (\acc x -> acc + f x) 0 xs

decodeChoice :: String -> Choice
decodeChoice s -- keep x, y, z around so part 1 still works
        | s == "A" || s == "X" = Rock
        | s == "B" || s == "Y" = Paper
        | s == "C" || s == "Z" = Scissors

dayTwoPartOne = withFile' "day2.txt" (calcStrategy (evaluate) . assumedStrategyGuide)

decodeResult :: String -> Result
decodeResult s
        | s == "X" = Lose
        | s == "Y" = Draw
        | s == "Z" = Win

decode :: (String, String) -> (Choice, Result)
decode (x, y) = (decodeChoice x, decodeResult y)

evaulate' :: (Choice, Result) -> Int
evaulate' (c, r) = case r of -- +1 as Enums start at 0
            Win -> (fromEnum $ next c) + 7
            Draw -> fromEnum c + 4
            Lose -> (fromEnum $ prev c) + 1

strategyGuide :: String -> [(Choice, Result)]
strategyGuide s = map (decode) . pairUp $ words s

dayTwoPartTwo = withFile' "day2.txt" (calcStrategy (evaulate') . strategyGuide)

-- Day 3 --



