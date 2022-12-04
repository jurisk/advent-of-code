module Main where

import Lib

import Data.Map (Map, (!))
import qualified Data.Map as Map

type RuleId = Int

data Rule = Literal Char
          | Sequence [RuleId]
          | EitherSequences [RuleId] [RuleId]
            deriving (Eq, Show)

parseRules :: [String] -> Map RuleId Rule
parseRules input = Map.fromList $ parseRule <$> input
  where
    parseRule :: String -> (RuleId, Rule)
    parseRule input = do
      let (h, t) = splitAtElem ':' input
      (read h, parse $ tail t)
      where
        parse :: String -> Rule
        parse ['"', ch ,'"'] = Literal ch
        parse s =
          if '|' `elem` s
          then uncurry EitherSequences $ mapTuple parseRuleIds $ splitAtElem '|' s
          else Sequence $ parseRuleIds s
          where
            parseRuleIds :: String -> [RuleId]
            parseRuleIds s = read <$> words s

process :: Map RuleId Rule -> [String] -> Int
process rules input = count matchesRule0 input
  where
    rule0 = rules ! 0
    matchesRule0 = validSolution . matches rule0

    validSolution :: Maybe [String] -> Bool
    validSolution Nothing = False
    validSolution (Just xs) = "" `elem` xs

    -- If this thing parses returns a `Just` with all possible unparsed remainders
    matches :: Rule -> String -> Maybe [String]

    matches (Literal ch) [] = Nothing
    matches (Literal ch) (x: xs) = if x == ch then Just [xs] else Nothing

    matches (Sequence []) input = Just [input]
    matches (Sequence (x : xs)) input = do
      first <- matches (rules ! x) input
      mergeMaybeLists $ matches (Sequence xs) <$> first

    matches (EitherSequences l r) input = f l <> f r
      where f x = matches (Sequence x) input

    mergeMaybeLists :: [Maybe [a]] -> Maybe [a]
    mergeMaybeLists list = foldl (<>) Nothing list

main :: IO ()
main = do
  inputLines <- readFileLines "src/Year2020/Day19/input.txt"
  let (ruleLines, tests) = splitAtElem [] inputLines
  let originalRules = parseRules ruleLines
  print $ process originalRules tests -- Part 1

  patchLines <- readFileLines "src/Year2020/Day19/patch.txt"
  let patchedRules = parseRules patchLines
  let merged = Map.union patchedRules originalRules -- Left-biased so patched rules have precedence
  print $ process merged tests -- Part 2
