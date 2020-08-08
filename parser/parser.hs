module Main where

import Control.Monad
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio

-- valid Scheme symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read Scheme expression
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

-- Scheme data types
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
  | Float Double
  | Rational (Ratio Integer)
  deriving (Show)

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
  return (Atom atom)

-- parser for Bool
parseBool :: Parser LispVal
parseBool = do
  char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

-- parser for Number
parseNumber :: Parser LispVal
parseNumber = parseNumberNormal <|> parseDec <|> parseOct  <|> parseHex <|> parseBin

parseNumberNormal :: Parser LispVal
parseNumberNormal = do
  digits <- many1 digit
  let n = read digits :: Integer
  return $ Number n

parseDec :: Parser LispVal
parseDec = do
  try (string "#d")
  digits <- many1 digit
  let n = read digits :: Integer
  return $ Number n

parseOct :: Parser LispVal
parseOct = do
  try (string "#o")
  s <- many1 octDigit
  let n = fst $ readOct s !! 0
  return $ Number n

parseHex :: Parser LispVal
parseHex = do
  try (string "#x")
  s <- many1 hexDigit
  let n = fst $ readHex s !! 0
  return $ Number n

parseBin :: Parser LispVal
parseBin = do
  try (string "#b")
  s <- many1 (oneOf "01")
  let n = binToDec s
  return $ Number n

binToDec :: String -> Integer
binToDec s = binToDec' $ reverse s
  where
    binToDec' "0" = 0
    binToDec' "1" = 1
    binToDec' (x : xs)
      | x == '0' = 2 * binToDec' xs
      | x == '1' = 1 + 2 * binToDec' xs

-- parser for character
parseChar :: Parser LispVal
parseChar = do
  try (string "#\\")
  char <- try (string "space" <|> string "newline") <|> 
    do{temp <- anyChar; notFollowedBy alphaNum; return [temp]}
  return $ Character $ case char of
    "space" -> ' '
    "newline" -> '\n'
    _ -> char !! 0

-- parser for float
parseFloat :: Parser LispVal
parseFloat = do
  integerPart <- try (many1 digit)
  char '.'
  fractionalPart <- many digit
  let doubleString = integerPart ++ "." ++ fractionalPart
  let out = fst $ readFloat doubleString !! 0
  return $ Float out

-- parser for rational
parseRational :: Parser LispVal
parseRational = do
  numerator <- try (many1 digit)
  char '/'
  denominator <- many1 digit
  let fraction = (readInt numerator) % (readInt denominator)
  return $ Rational fraction
  where
    readInt s = read s :: Integer

-- parser for List
parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

-- parser for Dotted List
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

-- parser for single-quote syntactic sugar in Scheme
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- parser for parenthesized list

-- parser for expression
parseExpr :: Parser LispVal
parseExpr = 
  try parseRational <|>
  try parseFloat <|>
  try parseAtom <|>
  try parseString <|>
  try parseNumber <|>
  try parseBool <|>
  try parseQuoted <|>
  do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x

-- test function to test parser
test :: String -> Parser LispVal -> String
test input parser = case parse (parser) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

-- main method
main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)

