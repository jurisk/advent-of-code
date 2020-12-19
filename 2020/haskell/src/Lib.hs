module Lib
    ( toInt
    , combinations
    , splitAtElem
    , count
    , mapTuple
    , readFileLines
    ) where

import Data.Maybe (fromJust)
import Data.List (elemIndex)

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
readFileLines file = lines <$> readFile file
