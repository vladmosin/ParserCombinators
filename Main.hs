module Main where

import Parser

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

main :: IO ()
main = do
  runParser "1 - 2 - 3 - 4 + gfj - 345 * fd - tr^2 "
  runParser " var = -23 * 123 + 17 + -4"
  runParser "-(i = 3)"
  runParser " -(3-4) "
  runParser " var123var = 98^Int"
  runParser "var = -two^2^21^bigInt"
  runParser " var = first_big_var * 978 + second_var"
  runParser " 2 * a / 4 * 12"
  runParser "--1*1"