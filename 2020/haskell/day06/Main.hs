module Main where

import Data.List

type MergeFunction = (String -> String -> String)
type TestCases = [[String]]

parseMultiLine :: String -> TestCases
parseMultiLine s = groupBy (\x y -> notNull x && notNull y) $ lines s
  where
    notNull x = not $ null x

calculateGroup :: MergeFunction -> [String] -> Int
calculateGroup f d = length $ foldl1 f d

calculate :: MergeFunction -> TestCases -> Int
calculate f d = sum $ calculateGroup f <$> d

solve :: TestCases -> MergeFunction -> IO ()
solve d f = print $ calculate f d

main = do
  rawData <- readFile "day06/input.txt"
  let parsed = parseMultiLine rawData
  mapM (solve parsed) [union, intersect]
