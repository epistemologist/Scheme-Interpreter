module Main where
import Text.ParserCombinators.Parsec hiding (spaces) -- import Parsec without the spaces function
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~" -- parser that matches one of the characters in the given string, and gives error otherwise

spaces :: Parser ()
spaces = skipMany1 space -- parser that recognizes 1 or more spaces

readExpr :: String -> String -- readExpr is a function that takes in a string and returns a string
readExpr input = case parse (spaces >> symbol) "lisp" input of -- input-parameter, "lisp" is the name of our input
-- note, we can write (spaces >> symbol) to pass input from spaces to symbol
-- generally, a >> b is equivalent to do a b
	Left err -> "No match: " ++ show err
	Right val -> "Found value: " ++ show val

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
				return $ String x -- return x

test :: String -> String
test input = case parse (parseString) "lisp" input of
			 Left err -> "No match: " ++ show err
			 Right val -> "Found value: " ++ val

main :: IO ()
main = do
	(expr:_) <- getArgs -- get the args and assign the first element to expr
	putStrLn (test expr)
