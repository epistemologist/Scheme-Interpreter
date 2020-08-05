module Main where
import Text.ParserCombinators.Parsec hiding (spaces) -- import Parsec without the spaces function
import System.Environment
import Control.Monad
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" -- parser that matches one of the characters in the given string, and gives error otherwise

spaces :: Parser ()
spaces = skipMany1 space -- parser that recognizes 1 or more spaces

readExpr :: String -> String -- readExpr is a function that takes in a string and returns a string
readExpr input = case parse parseExpr "lisp" input of -- input-parameter, "lisp" is the name of our input
	Left err -> "No match: " ++ show err
	Right val -> "Found value"

-- Defining a datatype for any kind of Lisp value (similar to an enum)
data LispVal = Atom String -- Atom: stores String naming the atom
			 | List [LispVal] -- List: stores a list of LispVal
			 | DottedList [LispVal] LispVal -- DottedList: improper list in Scheme, e.g. a pair
			 | Number Integer -- Number: Integer
			 | String String -- String: String
			 | Bool Bool -- Bool: Boolean

parseString :: Parser LispVal -- parser for a string
parseString = do -- do the following statements
				char '"' -- look for a single double quote
				x <- many (noneOf "\"") -- assign to x as many non-double quote characters as possible
				char '"' -- look for another single quote
				return (String x) -- return x casted as a String

parseAtom :: Parser LispVal -- parser for Atom
parseAtom = do -- do the following actions
			  first <- letter <|> symbol -- try to run letter and assign it to first, if that fails, then run symbol and assign it to first
			  rest <- many (letter <|> digit <|> symbol) -- recieve as many letters, digits, or symbols as there are
			  let atom = first:rest -- colon is list concatenation
			  return $ case atom of
			  			"#t" -> Bool True -- special case, "#t" is True in Scheme
			  			"#f" -> Bool False -- and "#f" is False in scheme
			  			_ -> Atom atom -- default case
{-
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
-- "many1 digit" - matches 1 or more digits
--  "(Number . read) $ many1 digit" == "Number read many1 digit" - 
-- note that many1 digit returns a Parser String so we need liftM to operate on (Number.read) for things to work
-}

-- Exercise 1: rewriting parseNumber
parseNumber :: Parser LispVal
parseNumber = do
			    digits <- many1 digit
			    let n = read digits :: Integer
			    return (Number n)

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

{-
test :: String -> String
test input = case parse (parseNumber) "lisp" input of
			 Left err -> "No match: " ++ show err
			 Right val -> "Found value: " ++ show val
-}

main :: IO ()
main = do
	(expr:_) <- getArgs -- get the args and assign the first element to expr
	putStrLn (readExpr expr)
