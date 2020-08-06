module Main where

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" 

spaces :: Parser ()
spaces = skipMany1 space 

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value"

data LispVal
  = Atom String 
  | List [LispVal] 
  | DottedList [LispVal] LispVal 
  | Number Integer 
  | String String 
  | Bool Bool 

escapedChars :: Parser Char
escapedChars = do
  char '\\' 
  x <- oneOf "\\\"" 
  return x

parseString :: Parser LispVal 
parseString = do
  
  char '"' 
  x <- many (escapedChars <|> noneOf "\\\"") 
  char '"' 
  return (String x) 

parseAtom :: Parser LispVal 
parseAtom = do
  first <- letter <|> symbol 
  rest <- many (letter <|> digit <|> symbol) 
  let atom = first : rest 
  return $ case atom of
    "#t" -> Bool True 
    "#f" -> Bool False 
    _ -> Atom atom 


parseNumber :: Parser LispVal
parseNumber = do
  digits <- many1 digit
  let n = read digits :: Integer
  return (Number n)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

test :: String -> String
test input = case parse (escapedChars) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

main :: IO ()
main = do
  (expr : _) <- getArgs 
  putStrLn (test expr)
