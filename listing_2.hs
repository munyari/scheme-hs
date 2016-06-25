module Main where
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- main :: IO ()
main = do
  args <- getArgs
  let num1_s = args !! 0
  num1 :: IO Int
  let num1 = read num1_s
  putStrLn("Hello, " ++ args !! 0)
