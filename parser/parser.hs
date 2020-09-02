module Main where

import Control.Monad
import Control.Monad.Except
import Numeric
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.Ratio
import Data.Typeable
import Data.Complex

-- valid Scheme symbols
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- read Scheme expression
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

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
  | Complex (Complex Double)
  deriving (Eq)

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
  fractionalPart <- many1 digit
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

-- parser for complex numbers
-- TODO work on this
parseComplex :: Parser LispVal
parseComplex = do
  real <- (try parseFloat <|> try parseNumberNormal)
  char '+'
  imag <- (try parseFloat <|> try parseNumberNormal)
  char 'j'
  let out = (toDouble real) :+ (toDouble imag)
  return $ Complex out
  where
   toDouble :: LispVal -> Double
   toDouble(Float x) = realToFrac x
   toDouble(Number n) = fromIntegral n
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

-- TODO: Implement custom show functions for all types
-- function to show LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")" 
showVal (Character c) = "\'" ++ show c ++ "\'"
showVal (Float f) = show f
showVal (Rational r) = show r
showVal (Complex c) = show c

-- helper function similar to " ".join() in Python
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- make LispVal a member of the type class Show
instance Show LispVal where show = showVal

-- start of evaluator
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val -- following line assigns input to val if val is type String
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Character _) = return val
eval val@(Float _) = return val
eval val@(Rational _) = return val
eval val@(Complex _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool False -> eval alt
    otherwise -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

-- array of primitive functions
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("symbol?", unaryFunction isAtom),
  ("integer?", unaryFunction isNumber),
  ("complex?", unaryFunction isComplex),
  ("real?", unaryFunction isFloat),
  ("float?", unaryFunction isFloat),
  ("rational?", unaryFunction isRational),
  ("boolean?", unaryFunction isBool),
  ("list?", unaryFunction isList),
  ("char?", unaryFunction isChar),
  ("string?", unaryFunction isString),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string<?", strBoolBinop (<)),
  ("string>?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eqv?", eqv)
  ]


-- utility function for f(Number, Number) -> Number
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op
-- numericBinop op params = Number $ foldl1 op $ map unpackNum params

-- utility function for f(LispVal, LispVal -> Bool
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op [l,r] = do
  unpackedL <- unpacker l
  unpackedR <- unpacker r
  return $ Bool $ unpackedL `op` unpackedR
boolBinop _ _ args = throwError $ NumArgs 2 args

-- Unpackers - functions that take LispVals and return their corresponding primitive types in Haskell
numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum  -- no weak typing

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (notString) = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b;
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool


-- utility function for a unary function
unaryFunction :: (LispVal -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
unaryFunction f [x] = f x

-- type-testing functions
isAtom, isList, isNumber, isString, isBool, isChar, isFloat, isRational, isComplex :: LispVal -> ThrowsError LispVal
isAtom (Atom _) = return $ Bool True
isAtom _ = return $ Bool False
isList (List _) = return $ Bool True
isList (DottedList _ _) = return $ Bool False
isList _ = return $ Bool False
isNumber (Number _) = return $ Bool True
isNumber _ = return $ Bool False
isString (String _) = return $ Bool True
isString _ = return $ Bool False
isBool (Bool _) = return $ Bool True
isBool _ = return $ Bool False
isChar (Character _) = return $ Bool True
isChar _ = return $ Bool False
isFloat (Float _) = return $ Bool True
isFloat _ = return $ Bool False
isRational (Rational _) = return $ Bool True
isRational _ = return $ Bool False
isComplex (Complex _) = return $ Bool True
isComplex _ = return $ Bool False

-- functions dealing with symbols
stringToSymbol :: LispVal -> LispVal
stringToSymbol (String s) = Atom $ s
stringToSymbol _ = Atom $ "" -- fix later with error handling

symbolToString :: LispVal -> LispVal
symbolToString (Atom s) = String $ s
symbolToString _ = String $ ""

-- List primitives
{-
1 - (car '(a b c)) = a
2 - (car '(a)) = a
3 - (car '(a b . c)) = a
4 - (car 'a) = error (arg isnt list)
5 - (car 'a 'b) = error (too many args)
-}

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x -- 1,2
car [DottedList (x:xs) _] = return x -- 3
car [badArg] = throwError $ TypeMismatch "pair" badArg -- 4
car badArgList = throwError $ NumArgs 1 badArgList -- 5

{-
1 - (cdr '(a b c)) = (b c)
2 - (cdr '(a b)) = (b)
3 - (cdr '(a)) = NIL
4 - (cdr '(a . b)) = b
5 - (cdr '(a b . c)) = (b . c)
6 - (cdr 'a) = error (arg not list)
7 - (cdr 'a 'b) = error (too many args)
-}

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs -- 1,2,3
cdr [DottedList [_] x] = return x  -- 4
cdr [DottedList (_:xs) x] = return $ DottedList xs x -- 5
cdr [badArg] = throwError $ TypeMismatch "pair" badArg -- 6
cdr badArgList = throwError $ NumArgs 1 badArgList -- 7

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []] = return $ List [x1] -- cons(anything, Nil) = 1 item list
cons [x, List xs] = return $ List $ x:xs -- cons(anything, list) puts anything at front of given list
cons [x, DottedList xs xlast] = return $ DottedList (x:xs) xlast -- case for dotted lists
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

-- function to test equality (strong typing!)
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left err -> False
      Right (Bool val) -> val

-- data type for error handling
data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected
  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
 ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

-- type that either throws LispError or returns value (similar to parse)
type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{-
Note: I don't really understand the functions used here in error evaluation - I wll use them as black boxes for now
-}


-- test function to test parser
test :: String -> Parser LispVal -> String
test input parser = case parse (parser) "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

-- main method
main :: IO ()
main = do
  args <- getArgs
  evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
  putStrLn $ extractValue $ trapError evaled
