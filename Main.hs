{-# LANGUAGE FlexibleInstances #-}

module Main where

import Parser
import Combinators (Result (Success, Error))

runParser :: String -> IO ()
runParser input = do
  putStrLn input
  print $ parse input
  putStrLn ""

instance {-# OVERLAPPING #-} Show a => Show (Maybe (Result a)) where
  show (Just (Success tree)) = show tree
  show (Just (Error err)) = "Syntax error: " ++ err
  show Nothing = "Empty tree"

main :: IO ()
main = do
  runParser "l_ist ++ [11, a = var ++ [kjkj], [ [], [   ]]   ]    "
  runParser "  var = [123]   "
  runParser "b = a ++ [[], 1,2,3,4]"
  runParser "   b    ; l ++ [     var = [fd, [ ]     ]]     ; lst = [12, 34, 54 * 47]; 16 * 48 "
  runParser "a = []; [b = 13, [z], 42 + 6] ++ a ++ [31, 25]; 77"