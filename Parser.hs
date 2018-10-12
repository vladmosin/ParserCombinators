module Parser (parse) where -- only expose the top-level parsing function

import Combinators as C
import qualified Tokenizer as T
import Prelude hiding (lookup, (>>=), map, pred, return, elem)

data AST = ASum T.Operator AST AST
         | AProd T.Operator AST AST
         | AAssign String AST
         | ANum Int
         | AIdent String
         | AList AST
         | ASem AST AST
         | AComma AST AST
         | ACon AST AST
         | AEmpty

-- TODO: Rewrite this without using Success and Error
parse :: String -> Maybe (Result AST)
parse input =
  case input of
    [] -> Nothing
    _  -> Just (result input)

result :: String -> (Result AST)
result input = 
  case statement input of
    Success (tree, ts') ->
      if null ts'
      then Success tree
      else Error ("Syntax error on: " ++ show ts') -- Only a prefix of the input is parsed
    Error err -> Error err -- Legitimate syntax error


statement :: Parser AST
statement = 
  ( base >>= \b ->
    semicolon |>
    statement >>= \st -> return (ASem b st)
  )
  <|> base

base :: Parser AST
base = 
  truelist
  <|> expression

listexpr :: Parser AST
listexpr =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    listexpr >>= \le -> return (AAssign i le)
  ) 
  <|> ( list     >>= \ll ->
        conc     |> 
        listexpr >>= \lr -> return (ACon ll lr)
  )
  <|> list

list :: Parser AST
list = 
  ( lsqbrac |>
    elements >>= \el ->
    rsqbrac |> return (AList el)
  )
  <|> ( lsqbrac |>
        rsqbrac |> return (AList (AEmpty))
      )
  <|> identifier

elements :: Parser AST
elements = 
  ( expression >>= \e ->
    comma |>
    elements >>= \el -> return (AComma e el)
  )
  <|>
  ( listexpr >>= \e ->
    comma |>
    elements >>= \el -> return (AComma e el)
  )
  <|> truelist
  <|> expression

truelist :: Parser AST
truelist = 
  ( lsqbrac |>
    elements >>= \el ->
    rsqbrac |> return (AList el)
  )
  <|> ( lsqbrac |>
        rsqbrac |> return (AList (AEmpty))
      )
  <|> ( list     >>= \ll ->
        conc     |> 
        listexpr >>= \lr -> return (ACon ll lr)
      )
  <|> ( identifier >>= \(AIdent i) ->
        assignment |>
        truelist >>= \le -> return (AAssign i le)
      ) 
expression :: Parser AST
expression =
  ( identifier >>= \(AIdent i) ->
    assignment |>
    expression >>= \e -> return (AAssign i e)
  )
  <|> ( term       >>= \l  -> -- Here the identifier is parsed twice :(
        plusMinus  >>= \op ->
        expression >>= \r  -> return (ASum op l r)
      )
  <|> term

term :: Parser AST
term =
  -- make sure we don't reparse the factor (Term -> Factor (('/' | '*') Term | epsilon ))
  factor >>= \l ->
  ( ( divMult >>= \op ->
      term    >>= \r  -> return (AProd op l r)
    )
    <|> return l
  )

factor :: Parser AST
factor =
  ( lparen |>
    expression >>= \e ->
    rparen |> return e -- No need to keep the parentheses
  )
  <|> identifier
  <|> digit

digit :: Parser AST
digit      = map (ANum   . T.pnum) (sat T.isNumber num)

identifier :: Parser AST
identifier = map (AIdent . T.word) (sat T.isWord ident)

lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

assignment :: Parser Char
assignment = char '='

plusMinus :: Parser T.Operator
plusMinus = map T.operator (char '+' <|> char '-')

divMult :: Parser T.Operator
divMult   = map T.operator (char '/' <|> char '*')

conc :: Parser String
conc = (sat (== "++") elem2)

lsqbrac :: Parser Char
lsqbrac = char '['

rsqbrac :: Parser Char
rsqbrac = char ']'

comma :: Parser Char
comma = char ','

semicolon :: Parser Char
semicolon = char ';'



instance Show AST where
  show tree = "\n" ++ show' 0 tree
    where
      show' n t =
        (if n > 0 then \s -> concat (replicate (n - 1) "| ") ++ "|_" ++ s else id)
        (case t of
                  ASum  op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AProd op l r -> showOp op : "\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AAssign  v e -> v ++ " =\n" ++ show' (ident n) e
                  ANum   i     -> show i
                  ACon l r     -> "++\n" ++ show' (ident n) l ++ "\n" ++ show' (ident n) r
                  AList l      -> "[\n" ++ show' (ident n) l ++ "]"
                  AComma l r   -> ",\n" ++ show' n l ++ "\n" ++ show' n r
                  AEmpty       -> ""
                  ASem l r     -> show' 0 l ++ "\n\n" ++ show' 0 r
                  AIdent i     -> show i)
      ident = (+1)
      showOp T.Plus  = '+'
      showOp T.Minus = '-'
      showOp T.Mult  = '*'
      showOp T.Div   = '/'