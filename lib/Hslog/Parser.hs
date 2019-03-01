module Hslog.Parser where

import Hslog.Types
import Hslog.Misc
import Control.Applicative hiding (many, some)
import Data.Maybe
import Data.Void
import Control.Monad.Combinators.Expr
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

-- Consumes spaces and comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "%"
    blockCmnt = L.skipBlockComment "/*" "*/" -- Correct in SWI-Prolog

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

-- Identifier part
idPart :: Parser String
idPart = many . oneOf $ validChars

atom :: Parser Atom
atom =
  (do
    idStart <- oneOf ['a'..'z']
    idRest <- idPart
    return $ Atom $ idStart : idRest
  ) <|> (do
    char '\''
    id <- some (noneOf "'")
    char '\''
    return $ Atom id
  )

num :: Parser Int
num = lexeme $ L.signed sc L.decimal

var :: Parser Var
var = lexeme $ do
  idStart <- oneOf ['A'..'Z'] <|> char '_'
  idRest <- idPart
  return $ Var $ idStart : idRest

compound :: Parser Compound
compound = do
  functor <- atom
  -- no spaces here
  symbol "("
  args <- termWithoutOperators `sepBy` symbol ","
  symbol ")"
  return $ Compound functor args

operatorTable :: [[Operator Parser Term]]
operatorTable =
  let
    makeComp str l = TComp (Compound (Atom str) l)
    makeBinComp str a b = makeComp str [a, b]
  in
  [ [ InfixL $ makeBinComp "=" <$ symbol "=" ] -- unification
  , [ InfixL $ makeBinComp "," <$ symbol "," ] -- conjuction
  , [ InfixL $ makeBinComp ";" <$ symbol ";" ] -- disjunction
  ]

term :: Parser Term
term = makeExprParser termWithoutOperators operatorTable

termWithoutOperators :: Parser Term
termWithoutOperators = try termWithParens <|> termWithoutParens
  where
    termWithoutParens = lexeme $
      try (TComp <$> compound)
      <|> try list
      <|> try (TAtom <$> atom)
      <|> try (TNum <$> num)
      <|> (TVar <$> var)

    termWithParens =
      symbol "(" *> term <* symbol ")"

    list = do
      symbol "["
      els <- termWithoutOperators `sepBy` symbol ","
      tail <- optional (symbol "|" *> term)
      symbol "]"
      let
        cons = Atom "."
        empty = TAtom (Atom "[]")
        default' = fromMaybe empty tail
      return $ foldr (\t acc -> TComp (Compound cons [t, acc])) default' els

phead :: Parser Head
phead = lexeme $
  try ((\(Compound a l) -> Head a l) <$> compound)
  <|> ((\a -> Head a []) <$> atom)

pbody :: Parser Body
pbody = lexeme $ Body <$> term

fact :: Parser Clause
fact = Fact <$> phead

rule :: Parser Clause
rule = do
  head' <- phead
  symbol ":-"
  Rule head' <$> pbody

clause :: Parser Clause
clause =
  sc *>
  (try rule <|> fact)
  <* symbol "."

query :: Parser Query
query =
  sc *> (Query <$> term) <* symbol "."

file :: Parser [Clause]
file = many clause <* eof
