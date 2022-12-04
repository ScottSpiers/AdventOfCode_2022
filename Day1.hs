module Main where

import System.IO
import Data.List
import Data.Ord

main = withFile' "day1.txt" part1


withFile' :: String -> (IO String -> IO ()) -> IO ()
withFile' path f = do
    handle <- openFile path ReadMode
    result <- f (hGetContents handle)
    hClose handle
    return result

part1 :: IO String -> IO ()
part1 contents = do
    c <- contents
    print . maximum . map (sum) . (map . map) (read :: String -> Int) $ split $ lines c

part2 :: IO String -> IO ()
part2 contents = do
    c <- contents
    let x = map (sum) . (map . map) (read :: String -> Int) $ split $ lines c
        in print $ sum $ take 3 (sortBy (comparing Down) x)




split :: [String] -> [[String]]
split s = case dropWhile (== "") s of
    [] -> []
    s' -> w : split s''
        where (w, s'') = break (== "") s'

