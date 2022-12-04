module Main where

import Lib

import Data.List
import Text.Parsec.String (Parser)
import Control.Monad (void)
import Text.Parsec (char, digit, string, between, many1, many, oneOf, (<|>), parse)
import Text.Parsec.Expr (buildExpressionParser, Assoc(AssocLeft), Operator(Infix))
import Control.Lens (Identity)

data Expression =   Number Int
                  | Add Expression Expression
                  | Multiply Expression Expression
                    deriving (Eq, Show)

-- http://jakewheat.github.io/intro_to_parsing/ was very helpful

parser1 :: Parser Expression
parser1 = buildExpressionParser table term
  where
    term  = parens parser1 <|> num
    table = [[binary "*" Multiply AssocLeft, binary "+" Add AssocLeft]] -- same precedence for * and +

parser2 :: Parser Expression
parser2 = buildExpressionParser table term
  where
    term  = parens parser2 <|> num
    table = [[binary "+" Add AssocLeft] -- + has higher precedence than * in this math
            ,[binary "*" Multiply AssocLeft]
            ]

binary :: String -> (a -> a -> a) -> Assoc -> Operator String () Identity a
binary name f = Infix (f <$ symbol name)
  where
    symbol :: String -> Parser String
    symbol s = lexeme $ string s

num :: Parser Expression
num = Number <$> integer
  where
    integer :: Parser Int
    integer = read <$> lexeme (many1 digit)

lexeme :: Parser a -> Parser a
lexeme p = p <* whitespace
  where
    whitespace :: Parser ()
    whitespace = void $ many $ oneOf " \t"

parens :: Parser a -> Parser a
parens = between openParen closeParen
  where
    openParen = lexeme $ char '('
    closeParen = lexeme $ char ')'

evaluate :: Expression -> Int
evaluate (Number x) = x
evaluate (Add a b) = evaluate a + evaluate b
evaluate (Multiply a b) = evaluate a * evaluate b

solve :: [String] -> Parser Expression -> IO ()
solve input parser = print $ sum $ evaluate . run parser <$> input
  where
    run :: Parser Expression -> String -> Expression
    run parser text = either (error . show) id $ parse parser text text

main :: IO [()]
main = do
  input <- readFileLines "src/Year2020/Day18/input.txt"
  mapM (solve input) [parser1, parser2]
