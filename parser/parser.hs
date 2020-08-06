module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

-- valid Scheme symbols
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read Scheme expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

-- Scheme data types
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool

-- escaped chars helper parser for parseString
escapedChars :: Parser Char
escapedChars = do
  char '\\'
  x <- oneOf "\\\"nrt"
  return $ case x of
  	'\\' -> x
  	'\"' -> x
  	'n' -> '\n'
  	'r' -> '\r'
  	't' -> '\t'

-- parser for String
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many (escapedChars <|> noneOf "\\\"")
  char '"'
  return (String x)

-- parser for Atom
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- parser for Number
parseNumber :: Parser LispVal
parseNumber = do
  digits <- many1 digit
  let n = read digits :: Integer
  return (Number n)

-- parser for expression
parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

-- test function to test parser
test :: String -> String
test input = case parse (escapedChars) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

-- main method
main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
