module Day05.Solution (task_1, task_2) where

import Lib.Common
import Text.Parsec
import Data.Functor.Identity (Identity)
import Data.Either.Combinators (mapLeft)
import Data.Functor ((<&>))
import Data.List (nub)

data Point = Point Int Int deriving (Show, Eq)
data Vent = Vent Point Point deriving (Show, Eq)

int :: ParsecT String u Identity Int
int = rd <$> many1 digit
    where rd = read :: String -> Int

pointParser :: ParsecT String u Identity Point
pointParser = do
  a <- int
  _ <- char ','
  b <- int
  return $ Point a b

ventParser :: ParsecT String u Identity Vent
ventParser = do
  a <- pointParser
  _ <- string " -> "
  b <- pointParser
  _ <- newline
  return $ Vent a b

ventsParser :: ParsecT String u Identity [Vent]
ventsParser = many1 ventParser

showError :: ParseError -> String
showError = show

parseVents :: String -> Either String [Vent]
parseVents input = mapLeft showError result
  where result = parse ventsParser "(unknown)" input

horizontalOrVertical :: Vent -> Bool
horizontalOrVertical (Vent (Point x1 y1) (Point x2 y2)) = x1 == x2 || y1 == y2

diagonal :: Vent -> Bool
diagonal (Vent (Point x1 y1) (Point x2 y2)) = abs (x1 - x2) == abs (y1 - y2) 

horizontalOrVerticalOrDiagonal :: Vent -> Bool
horizontalOrVerticalOrDiagonal v = horizontalOrVertical v || diagonal v

fromTo :: Int -> Int -> [Int]
fromTo a b 
  | a < b = [min a b..max a b]
  | otherwise = reverse [min a b..max a b]

allPointsForVent :: Vent -> [Point]
allPointsForVent (Vent (Point x1 y1) (Point x2 y2))
  | x1 == x2 = fromTo y1 y2 <&> Point x1
  | y1 == y2 = fromTo x1 x2 <&> (`Point` y1)
  | diagonal (Vent (Point x1 y1) (Point x2 y2)) = fromTo x1 x2 `zip` fromTo y1 y2 <&> uncurry Point
  | otherwise = error "Expected horizontal or vertical lines"
  
-- Not optimal - O(n^2) - but works. Defining Ord for Point and using a Map would be more efficient.
solve :: (Vent -> Bool) -> [Vent] -> Int
solve p vents = length multiCrossingPoints
  where
    multiCrossingPoints = filter isMultiCrossing distinctPoints
    isMultiCrossing point = length (filter (== point) allPoints) >= 2
    distinctPoints = nub allPoints
    allPoints = relevantVents >>= allPointsForVent
    relevantVents = filter p vents

solve_1 :: [Vent] -> Int
solve_1 = solve horizontalOrVertical

solve_2 :: [Vent] -> Int
solve_2 = solve horizontalOrVerticalOrDiagonal

task_1 :: String -> Either ErrorMessage Int
task_1 input = solve_1 <$> parseVents input

task_2 :: String -> Either ErrorMessage Int
task_2 input = solve_2 <$> parseVents input
