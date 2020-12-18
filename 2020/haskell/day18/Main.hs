module Main where

import Data.List

type Parser = String -> Expression

data Expression =   Number Int
                  | Add Expression Expression
                  | Multiply Expression Expression
                    deriving (Show)

parse1 :: String -> Expression
parse1 x = Number 0 -- TODO: implement

parse2 :: String -> Expression
parse2 x = Number 0 -- TODO: implement

evaluate :: Expression -> Int
evaluate (Number x) = x
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Multiply a b) = evaluate a * evaluate b

solve :: [String] -> Parser -> IO ()
solve input parser = print $ sum $ evaluate . parser <$> input

main = do
  rawData <- readFile "day18/18.txt"
  let input = lines rawData
  mapM (solve input) [parse1, parse2]
