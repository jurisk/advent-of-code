module Main where

import Lib

solve :: Int -> [Int] -> Int
solve n list = product $ head $ filter (\x -> sum x == 2020) (combinations n list)

main :: IO ()
main = do
  input <- readFileLines "day01/input.txt"
  let parsed = toInt <$> input
  print $ solve 2 parsed
  print $ solve 3 parsed
