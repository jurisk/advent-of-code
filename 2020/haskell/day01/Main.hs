module Main where

import Lib

solve :: Int -> [Int] -> Int
solve n list = product $ head $ filter (\x -> sum x == 2020) (combinations n list)

main :: IO ()
main = do
  raw <- readFile "day01/01.txt"
  let input = toInt <$> lines raw
  print $ solve 2 input
  print $ solve 3 input
