{-# OPTIONS_GHC -XTypeFamilies -XGADTs -XFlexibleContexts #-}
module Main where
import qualified Text.Parsec as Parsec
import Control.Applicative

data Expr = Id String | Num Integer | Range Integer Integer | FuncExpr String [Expr] | FuncLit [String] [Stmt] deriving (Show)
data NoaType = TCons String TypeTail deriving (Show)
data TypeTail = Tail NoaType | NilTail deriving (Show)
data Stmt = Assign String Expr NoaType | Foreach Expr [Stmt] | FuncStmt String [Expr] | If Expr [Stmt] [Stmt] deriving (Show)
parse rule text = Parsec.parse rule "(source)" text

dblcolon :: Parsec.Parsec String () String
dblcolon = Parsec.count 2 $ Parsec.char ':'

dblperiod :: Parsec.Parsec String () String
dblperiod = Parsec.count 2 $ Parsec.char '.'

ident :: Parsec.Parsec String () String
ident = do
  l <- Parsec.letter
  m <- Parsec.many $ Parsec.letter <|> Parsec.digit
  Parsec.spaces
  return $ [l]++m

num :: Parsec.Parsec String () String
num = Parsec.many1 Parsec.digit

delim :: Parsec.Parsec String () Char
delim = do
  Parsec.spaces
  Parsec.char ';'
  Parsec.spaces
  return ';'
  
range :: Parsec.Parsec String () Expr
range = do
  Parsec.char '['
  l<-num
  dblperiod
  m<-num
  Parsec.char ']'
  return $ Range (read l) $ read  m

exprIdent :: Parsec.Parsec String () Expr
exprIdent = do
  m <- ident
  return $ Id m

exprNum :: Parsec.Parsec String () Expr
exprNum = do
  m <- num
  return $ Num $ read m

funccall :: Parsec.Parsec String () (String,[Expr])
funccall = do
  s<-ident
  Parsec.spaces
  Parsec.char '('
  Parsec.spaces
  t<-Parsec.sepBy expr $ Parsec.char ','
  Parsec.spaces
  Parsec.char ')'
  Parsec.spaces
  return (s,t)

funccallExpr :: Parsec.Parsec String () Expr
funccallExpr = do
  (a,b)<-funccall
  return $ FuncExpr a b
  
funccallStmt :: Parsec.Parsec String () Stmt
funccallStmt = do
  (a,b)<-funccall
  return $ FuncStmt a b

assignStmt :: Parsec.Parsec String () Stmt
assignStmt = do
  val <- ident
  Parsec.spaces
  Parsec.char '='
  Parsec.spaces
  e <- expr
  Parsec.spaces
  dblcolon
  Parsec.spaces
  t <- parseType
  Parsec.spaces
  return $ Assign val e t

foreachStmt :: Parsec.Parsec String () Stmt
foreachStmt = do
  Parsec.string "foreach"
  Parsec.spaces
  e <- expr
  Parsec.spaces
  Parsec.char '{'
  Parsec.spaces
  s <- Parsec.endBy1 stmt delim
  Parsec.spaces
  Parsec.char '}'
  Parsec.spaces
  return $ Foreach e s

parens :: Parsec.Parsec String () Expr
parens = do
  Parsec.char '('
  Parsec.spaces
  e<-expr
  Parsec.spaces
  Parsec.char ')'
  Parsec.spaces
  return e


parensType :: Parsec.Parsec String () NoaType
parensType = do
  Parsec.char '('
  Parsec.spaces
  t<-parseType
  Parsec.spaces
  Parsec.char ')'
  Parsec.spaces
  return t

idType :: Parsec.Parsec String () NoaType
idType = do
  t<-ident
  Parsec.spaces
  s<-typeTail
  Parsec.spaces
  return $ TCons t s

fullTail :: Parsec.Parsec String () TypeTail
fullTail = do
  Parsec.string "->"
  Parsec.spaces
  t<-parseType
  Parsec.spaces
  return $ Tail t

nilTail :: Parsec.Parsec String () TypeTail
nilTail = do
  return NilTail
  
typeTail :: Parsec.Parsec String () TypeTail
typeTail = Parsec.try fullTail <|> nilTail

parseType :: Parsec.Parsec String () NoaType
parseType = idType <|> parensType

funcLit :: Parsec.Parsec String () Expr
funcLit = do
  Parsec.char '{'
  Parsec.spaces
  s<-Parsec.many ident
  Parsec.spaces
  Parsec.string "->"
  Parsec.spaces
  t <- Parsec.endBy stmt delim
  Parsec.spaces
  Parsec.char '}'
  Parsec.spaces
  return $ FuncLit s t

ifStmt :: Parsec.Parsec String () Stmt
ifStmt = do
  Parsec.string "if"
  Parsec.spaces
  e<-expr
  Parsec.spaces
  Parsec.char '{'
  Parsec.spaces
  s<-Parsec.endBy stmt delim
  Parsec.spaces
  Parsec.char '}'
  Parsec.spaces
  Parsec.string "else"
  Parsec.spaces
  Parsec.char '{'
  Parsec.spaces
  t<-Parsec.endBy stmt delim
  Parsec.spaces
  Parsec.char '}'
  Parsec.spaces
  return $ If e s t
  
expr :: Parsec.Parsec String () Expr
expr = parens <|> range <|> Parsec.try funccallExpr <|> funcLit <|> exprIdent <|> exprNum

stmt :: Parsec.Parsec String () Stmt
stmt = ifStmt <|> foreachStmt <|> assignStmt <|> funccallStmt

prog :: Parsec.Parsec String () [Stmt]
prog = do
  Parsec.spaces
  s<-Parsec.endBy stmt delim
  return s

main :: IO ()
main = putStrLn $ show $ parse assignStmt "a={->}::a->(b);"
