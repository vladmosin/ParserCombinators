module Tokenizer where

data Token = TNumber Int
           | TIdent String
           | TOp Operator
           | TLParen
           | TRParen
           | TAssign
           | TEof
           deriving (Show, Eq)

data Operator = Plus
              | Pow
              | Minus
              | Mult
              | Div
              deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize [] = [TEof]
tokenize (c : cs) | isOperator c   = TOp (operator c) : tokenize cs
                  | isDigit c      = TNumber (number (reverse (digits (c : cs)))) : tokenize (drop (numlen cs) cs)
                  | isAlpha c      = TIdent (c : (word cs)) : tokenize (drop (wordlen cs) cs)
                  | c == '('       = TLParen : tokenize cs
                  | c == ')'       = TRParen : tokenize cs
                  | c == '='       = TAssign : tokenize cs
                  | isWhiteSpace c = tokenize cs
                  | otherwise = error ("Lexical error: unacceptable character " ++ [c])

isOperator :: Char -> Bool
isOperator x = x `elem` "+-*/^"

soperator :: String -> Operator
soperator (c : cs) = operator c

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '^' = Pow
           | c == '-' = Minus
           | c == '*' = Mult
           | c == '/' = Div
operator c = error ("Lexical error: " ++ c : " is not an operator!")

isDigit :: Char -> Bool
isDigit c = c `elem` "0123456789"

isNumber :: String -> Bool
isNumber [] = False
isNumber (c : cs) = c `elem` "0123456789" && (areDigits cs)

areDigits :: String -> Bool
areDigits [] = True
areDigits (c:cs) = (c `elem` "0123456789") && (areDigits cs)


digit :: Char -> Int
digit c | c == '0' = 0
        | c == '1' = 1
        | c == '2' = 2
        | c == '3' = 3
        | c == '4' = 4
        | c == '5' = 5
        | c == '6' = 6
        | c == '7' = 7
        | c == '8' = 8
        | c == '9' = 9
digit c = error ("Lexical error: " ++ c : " is not a digit!")

digits :: String -> [Int]
digits [] = []
digits (c : cs) | isDigit c = digit c : digits cs
                | otherwise = []
                
number :: [Int] -> Int
number [] = 0
number (num : nums) = num + 10 * number(nums)

numlen :: String -> Int
numlen [] = 0
numlen (c : cs)  | isDigit c = 1 + numlen cs
                 | otherwise = 0

pnum :: String -> Int
pnum cs = number (reverse (digits (cs)))

isAlpha :: Char -> Bool
isAlpha c = c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z']

wordlen :: String -> Int
wordlen [] = 0
wordlen (c : cs) | isDigit c = 1 + wordlen cs
                 | isAlpha c = 1 + wordlen cs
                 | c == '_'  = 1 + wordlen cs
                 | c == '$'  = 1 + wordlen cs
                 | otherwise = 0

isWord :: String -> Bool
isWord [] = False
isWord (c : cs) | isAlpha c = True
                | otherwise = False

                 
word :: String -> String
word [] = []
word (c : cs) | isDigit c = c : word cs
              | isAlpha c = c : word cs
              | c == '_'  = c : word cs
              | c == '$'  = c : word cs
              | otherwise = []

alpha :: Char -> Char
alpha c = c

isWhiteSpace :: Char -> Bool
isWhiteSpace c = c `elem` " \t\n"

isStopSymb :: Char -> Bool
isStopSymb c = c `elem` ",;]"
