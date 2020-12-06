module Advent01 where

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

toInt :: String -> Int
toInt x = read x :: Int

solve :: Int -> [Int] -> Int
solve n list = product $ head $ filter (\x -> sum x == 2020) (combinations n list)

main :: IO ()
main = do
  raw <- readFile "01.txt"
  let input = toInt <$> lines raw
  print $ solve 2 input
  print $ solve 3 input
