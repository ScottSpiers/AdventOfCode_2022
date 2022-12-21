module Main where

import System.IO
import Data.Char
import Data.List
import Data.Ord

main = do 
    dayOnePartOne
    dayOnePartTwo
    dayTwoPartOne
    dayTwoPartTwo
    dayThreePartOne
    dayThreePartTwo
    dayFourPartOne
    dayFourPartTwo

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
elvesTotalCalories s = map sum . mapRead . split $ lines s

maxCalories :: String -> Int
maxCalories s = maximum $ elvesTotalCalories s

dayOnePartOne = withFile' "day1.txt" maxCalories

top3Calories :: String -> Int
top3Calories s = (sum . take 3 . sortBy (comparing Down)) $ elvesTotalCalories s

dayOnePartTwo = withFile' "day1.txt" top3Calories


-- Day 2 --

data Choice = Rock | Paper | Scissors deriving (Bounded, Enum, Eq, Show)

data Result = Lose | Draw | Win deriving (Enum, Show)


pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:[]) = []
pairs (x:y:xs) = (x, y) : pairs xs

next :: (Bounded a, Enum a, Eq a) => a -> a
next x 
        | x == maxBound = minBound
        | otherwise = succ x

prev :: (Bounded a, Enum a, Eq a) => a -> a
prev x 
        | x == minBound = maxBound
        | otherwise = pred x

calcStrategy :: (Enum a, Enum b) => ((a, b) -> Int) -> [(a, b)] -> Int
calcStrategy f xs = foldl (\acc x -> acc + f x) 0 xs

evaluate :: (Choice, Choice) -> Int
evaluate (x, y) -- +1 as Enums start at 0
    | x == y = fromEnum y + 4
    | x == prev y = fromEnum y + 7
    | otherwise = fromEnum y + 1

assumedStrategyGuide :: String -> [(Choice, Choice)]
assumedStrategyGuide s = pairs . map (decodeChoice) $ words s


decodeChoice :: String -> Choice
decodeChoice s -- keep x, y, z around so part 1 still works
        | s == "A" || s == "X" = Rock
        | s == "B" || s == "Y" = Paper
        | s == "C" || s == "Z" = Scissors

dayTwoPartOne = withFile' "day2.txt" (calcStrategy evaluate . assumedStrategyGuide)

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
strategyGuide s = map (decode) . pairs $ words s

dayTwoPartTwo = withFile' "day2.txt" (calcStrategy evaulate' . strategyGuide)


-- Day 3 --

getCharIndex :: Char -> Int
getCharIndex c
        | idx < 26 = (mod (idx + 26) 52) + 1
        | otherwise = (mod (idx + 20) 52) + 1 --offset of 6 chars
        where idx = ord c - ord 'A'

getPriority :: String -> Int
getPriority s = sum . map getCharIndex $ nub [c | c <- x, c `elem` y]
        where (x, y) = splitAt (length s `div` 2) s

getTotalPriority :: [String] -> Int
getTotalPriority s = sum $ map getPriority s

dayThreePartOne = withFile' "day3.txt" (getTotalPriority . lines)

elfGroups :: [String] -> [[String]]
elfGroups [] = []
elfGroups s =  x : elfGroups y
        where (x, y) = splitAt 3 s

getBadgePriority :: [String] -> Int
getBadgePriority (x:y:z:xs) = sum . map getCharIndex $ nub [c | c <- x, c `elem` y, c `elem` z]
getBadgePriority _ = 0

getTotalBadgePriority :: [[String]] -> Int
getTotalBadgePriority s = sum $ map getBadgePriority s

dayThreePartTwo = withFile' "day3.txt" (getTotalBadgePriority . elfGroups . lines)

-- Day 4 --

dayFourPartOne = withFile' "day4.txt" (numContained . lines)

splitBy :: (Char -> Bool) -> String -> (String, String)
splitBy p s = (x, drop (length x + 1) s)
        where x = takeWhile p s

fullyContains :: ((Int, Int), (Int, Int)) -> Bool
fullyContains ((w, x), (y, z)) = elf1 `isInfixOf` elf2 || elf2 `isInfixOf` elf1
        where 
                elf1 = [w..x]
                elf2 = [y..z]

parseGroups :: (String, String) -> ((Int, Int), (Int, Int))
parseGroups (s1, s2) = ((read $ fst p1, read $ snd p1), (read $ fst p2, read $ snd p2))
        where 
                p1 = splitBy (/= '-') s1
                p2 = splitBy (/= '-') s2

groups = map parseGroups . map (splitBy (/= ','))

numContained = length . filter fullyContains . groups

dayFourPartTwo = withFile' "day4.txt" (numContained' . lines)

containsAny :: ((Int, Int), (Int, Int)) -> Bool
containsAny ((w, x), (y, z)) = length (elf1 \\ elf2) /= length elf1 || length (elf2 \\ elf1) /= length elf2
        where 
                elf1 = [w..x]
                elf2 = [y..z]

numContained' = length . filter containsAny . groups

-- Day 5 --

data Action = Move | Add | None deriving (Enum, Show, Eq)

data Direction = From | To | Other deriving (Enum, Show, Eq)
        

test = map (reverse . tail) . groupBy (\x -> isAlpha) . filter (isAlphaNum) . unlines . transpose . reverse . test2

test2 = takeWhile (/= "") . lines

state :: String -> [String]
state s = performActions y x
        where (x, y) = (test s, instructions s)

instructions = mapRead . map (filter (isDigit')) . map words . tail . dropWhile (/="") . lines

dayFivePartOne = withFile' "day5.txt" (getTops .state)

dayFivePartTwo = withFile' "day5.txt" (getTops .state')

test3 = withFile' "day5.txt" instructions

test4 = withFile' "day5.txt" test

getTops :: [String] -> String
getTops [] = []
getTops ("":xs) = " " ++ getTops xs
getTops (x:xs) = head x : getTops xs

isDigit' :: String -> Bool
isDigit' s = all isDigit s

doMove :: [Int] -> [String] -> [String]
doMove (m:f:t:[]) xs
        | f < t = x ++ ((drop m idx) : tail w) ++ ((s ++ tSt) : tail z)
        | f > t = a ++ ((s ++ tSt) : tail c) ++ ((drop m idx) : tail d)
        | otherwise = xs --moving from x to x while result in same string? or does it reverse the take
        where
                s = reverse $ take m idx
                tSt = xs !! (t - 1)
                idx = xs !! (f - 1)
                (x, y) = splitAt (f-1) xs
                (w, z) = splitAt (abs(f-t)) y
                (a, b) = splitAt (t-1) xs
                (c, d) = splitAt (abs(f-t)) b
doMove _ xs = xs

state' :: String -> [String]
state' s = performActions' y x
        where (x, y) = (test s, instructions s)

doMove' :: [Int] -> [String] -> [String]
doMove' (m:f:t:[]) xs
        | f < t = x ++ ((drop m idx) : tail w) ++ ((s ++ tSt) : tail z)
        | f > t = a ++ ((s ++ tSt) : tail c) ++ ((drop m idx) : tail d)
        | otherwise = xs --moving from x to x while result in same string? or does it reverse the take
        where
                s = take m idx
                tSt = xs !! (t - 1)
                idx = xs !! (f - 1)
                (x, y) = splitAt (f-1) xs
                (w, z) = splitAt (abs(f-t)) y
                (a, b) = splitAt (t-1) xs
                (c, d) = splitAt (abs(f-t)) b
doMove' _ xs = xs

performActions :: [[Int]] -> [String] -> [String]
performActions (x:[]) s = doMove x s
performActions (x:xs) s = performActions xs (doMove x s)

performActions' :: [[Int]] -> [String] -> [String]
performActions' (x:[]) s = doMove' x s
performActions' (x:xs) s = performActions' xs (doMove' x s)