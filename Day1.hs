module Main where

import System.IO

main = withFile' "day1.txt" day1


withFile' :: String -> (IO String -> IO ()) -> IO ()
withFile' path f = do
    handle <- openFile path ReadMode
    result <- f (hGetContents handle)
    hClose handle
    return result

day1 :: IO String -> IO ()
day1 contents = do
    c <- contents
    print . maximum . map (sum) . (map . map) (read :: String -> Int) $ split $ lines c

split :: [String] -> [[String]]
split s = case dropWhile (== "") s of
    [] -> []
    [""] -> split s
    s' -> w : split s''
        where (w, s'') = break (== "") s'

