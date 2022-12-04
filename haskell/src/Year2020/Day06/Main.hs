module Main where

import Lib

import Data.List (union, intersect)

type MergeFunction = (String -> String -> String)
type TestCases = [[String]]

calculateGroup :: MergeFunction -> [String] -> Int
calculateGroup f d = length $ foldl1 f d

calculate :: MergeFunction -> TestCases -> Int
calculate f d = sum $ calculateGroup f <$> d

solve :: TestCases -> MergeFunction -> IO ()
solve d f = print $ calculate f d

main :: IO [()]
main = do
  input <- readFileMultiLines "src/Year2020/Day06/input.txt"
  mapM (solve input) [union, intersect]
