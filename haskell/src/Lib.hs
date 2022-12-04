module Lib
    ( toInt
    , combinations
    , splitAtElem
    , count
    , mapTuple
    , readFileLines
    , readFileMultiLines
    , assertEqualsM
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex, groupBy)

toInt :: String -> Int
toInt x = read x :: Int

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

splitAtElem :: Eq a => a -> [a] -> ([a], [a])
splitAtElem elem list = do
  let index = fromJust $ elemIndex elem list
  let (h, t) = splitAt index list
  (h, tail t)

count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (l, r) = (f l, f r)

readFileLines :: String -> IO [String]
readFileLines fileName = lines <$> readFile fileName

readFileMultiLines :: String -> IO [[String]]
readFileMultiLines fileName = do
  text <- readFile fileName
  let textLines = lines text
  let grouped = groupBy notNullBoth textLines
  let filtered = filter notSeparator grouped
  return filtered
    where
      notNullBoth x y = notNull x && notNull y
      notNull = not . null
      notSeparator = (/=) [""]

assertEqualsM :: (Eq a, Show a) => a -> a -> IO ()
assertEqualsM obtained expected =
  if obtained == expected
  then pure ()
  else error ("Obtained " ++ show obtained ++ " but expected " ++ show expected)
