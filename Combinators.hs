module Combinators where
-- Make sure that the names don't clash
import Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

-- Input abstraction
type Input = String

-- Result is polymorphic in the ... result
data Result r = Success r
              | Error String
              deriving (Show)

-- The result of parsing is some payload r and the suffix which wasn't parsed
type Parser r = Input -> Result (r, Input)

-- Choice combinator: checks if the input can be parsed with either the first, or the second parser
-- Left biased: make sure, that the first parser consumes more input
infixl 6 <|>
(<|>) :: Parser a -> Parser a -> Parser a
p <|> q = \inp ->
  case p (skip inp) of
    Error _ -> q (skip inp)
    result  -> result

skip:: String -> String
skip [] = []
skip (c : cs) | T.isWhiteSpace c = skip cs
              | otherwise = c:cs

-- Sequential combinator: if the first parser successfully parses some prefix, the second is run on the suffix
-- The second parser is supposed to use the result of the first parser
infixl 7 >>=
(>>=) :: Parser a -> (a -> Parser b ) -> Parser b
p >>= q = \inp ->
  case p (skip inp) of
    Success (r, inp') -> q r (skip inp')
    Error err -> Error err

chend :: Parser a -> Parser a -> Parser a
chend p q = \inp ->
  case p (skip inp) of 
    Error _ -> q (skip inp)
    Success (r, inp') ->
      case (skip inp') of
        null -> Success (r, skip inp')
        c : cs | T.isStopSymb c ->  Success (r, skip inp')
        _ -> q (skip inp)

-- Sequential combinator which ignores the result of the first parser
infixl 7 |>
(|>) :: Parser a -> Parser b -> Parser b
p |> q = p >>= const q

-- Succeedes without consuming any input, returning a value
return :: a -> Parser a
return r inp = Success (r, inp)

-- Always fails
zero :: String -> Parser a
zero err = const $ Error err

-- Chops of the first element of the string
elem :: Parser Char
elem (c : cs) = Success (c, cs)
elem [] = Error "Empty string"

elem2 :: Parser String
elem2 [] = Error "Empty string"
elem2 (c : cs) = 
    let res = elem cs in
    case res of
        Success (r, inp) -> Success (c : r : [], inp)
        Error err -> Error err



num :: Parser String
num [] = Error "No symbols"
num (c : cs) | T.isDigit c = 
                let (s1, s2) = num' cs in 
                    Success(c : s1, s2)
             | otherwise = Error ("Not Number" ++ show c ++ show cs)

num' :: String -> (String, String)
num' [] = ([], [])
num' (c : cs) | T.isDigit c =
                let (s1, s2) = num' cs in
                            (c : s1, s2)
              | otherwise = ([], c : cs) 

ident :: Parser String
ident [] = Error "No symbols"
ident (c : cs) | T.isAlpha c = 
                    let (s1, s2) = word' cs in
                            Success(c : s1, s2)
              | otherwise = Error "Not letter"

word' :: String -> (String, String)
word' [] = ([], [])
word' (c : cs) | T.isAlpha c =
                    let (s1, s2) = word' cs in
                            (c : s1, s2)
               | T.isDigit c =
                    let (s1, s2) = word' cs in
                            (c : s1, s2)
               | c == '_' =
                    let (s1, s2) = word' cs in
                            (c : s1, s2)
               | c == '$' =
                    let (s1, s2) = word' cs in
                            (c : s1, s2)
               | otherwise = ([], c : cs)

-- Checks if the first character of the string is the given one
char :: Char -> Parser Char
char c = sat (== c) elem

-- Checks if the parser result satisfies the predicate
sat :: (a -> Bool) -> Parser a -> Parser a
sat pred parser inp =
  case parser inp of
    Success (r, inp') | pred r ->  Success (r, inp')
    Success _ -> Error "Predicate is not satisfied"
    Error err -> Error err

-- Applies the function to the result of the parser
map :: (a -> b) -> Parser a -> Parser b
map f parser inp =
  case parser inp of
    Success (r, inp') -> Success (f r, inp')
    Error err -> Error err