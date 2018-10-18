{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Result (a, b)) where
  show (Success (tree, inp)) = show tree
  show (Error err) = "Syntax error: " ++ err

main :: IO ()
main = do
  runParser "l_ist ++ [11, a = var ++ [kjkj], [ [], [   ]]   ]    "
  runParser "  var = [123]   "
  runParser "b = a ++ [[], 1,2,3,4]"
  runParser "   b    ; l ++ [     var = [fd, [ ]     ]]     ; lst = [12, 34, 54 * 47]; 16 * 48 "
  runParser "a = []; [b = 13, [z], 42 + 6] ++ a ++ [31, 25]; 77"
  runParser "[456] ++ [123]"
  runParser "1 * 2 - 3 / 4 + (-5) ^ 73; width = 14 +128 * -length                "
  runParser "- ( 3- 4) "
  runParser "1 - 2 - 3 + 4 * 5 /4 / 8 + 3^    5^    6 "
  runParser "[1 + 2, 3 + 4, [7, 8, 9]] ++ [a]; x = []; [1, 2, 4]; var = w^fj           "
  runParser "jfg + fg    "