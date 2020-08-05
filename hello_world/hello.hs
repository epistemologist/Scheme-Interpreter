module Main where -- creates module named Main, used for every Haskell program
import System.Environment -- imports IO functions

main :: IO () -- main method
main = do -- main function, do keyword followed by list of actions;
	args <- getArgs -- run getArgs and then assign the result to args
	putStrLn ("Hello, " ++ args !! 0) -- run putStrLn on "Hello" + args[0]
