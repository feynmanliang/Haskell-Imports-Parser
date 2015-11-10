{-# OPTIONS -Wall -fwarn-tabs #-}
module ImportParser where

-- | Demonstrates scoping backtracking `try`s so Parsec
-- can produce more meaningful localized failures by only
-- backtracking when appropriate

import Data.Functor.Identity
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)

data Stmt = QualifiedImport String String
  | Import String
  deriving (Show)

lexer :: P.GenTokenParser String u Identity
lexer = P.makeTokenParser (haskellDef
  { P.reservedNames = P.reservedNames haskellDef ++ ["qualified", "as"] })
identifier :: ParsecT String u Identity String
identifier = P.identifier lexer
reserved :: String -> ParsecT String u Identity ()
reserved = P.reserved lexer

pQualifiedImport :: ParsecT String u Identity Stmt
pQualifiedImport = do
  -- scope of backtracking from `try` localized into pQualifiedImport
  -- this prevents backtracking after "import qualified" are parsed
  try $ do
    reserved "import"
    reserved "qualified"
  i <- identifier
  reserved "as"
  i' <- identifier
  return (QualifiedImport i i')

pImport :: ParsecT String u Identity Stmt
pImport = do
  reserved "import"
  i <- identifier
  return (Import i)

pStmt :: ParsecT String u Identity Stmt
-- if we did `try pQualifiedImport` instead, then
-- `parseStmt "import qualified Foo s X" would backtrack
-- all the way back to the <|> and try pImport parser
pStmt = pQualifiedImport <|> pImport

parseStmt :: String -> Either ParseError Stmt
parseStmt = parse pStmt "(unknown)"
