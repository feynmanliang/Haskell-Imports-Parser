{-# OPTIONS -Wall -fwarn-tabs #-}
module CSVParser where

import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec

csvFile :: ParsecT String u Identity [[String]]
csvFile = endBy line eol

line :: ParsecT String u Identity [String]
line = sepBy cell (char ',')

cell :: ParsecT String u Identity String
cell = quotedCell <|> many (noneOf ",\n")

quotedCell :: ParsecT String u Identity String
quotedCell =
  do _ <- char '"'
     content <- many quotedChar
     _ <- char '"' <?> "quote at end of cell"
     return content

quotedChar :: ParsecT String u Identity Char
quotedChar =
  noneOf "\""
  <|> try (string "\"" >> return '"')

eol :: ParsecT String u Identity String
eol = try (string "\n\r")
  <|> try (string "\r\n")
  <|> string "\n"
  <|> string "\r"
  <?> "end of line"

parseCSV :: String -> Either ParseError [[String]]
parseCSV = parse csvFile "(unknown)"

main :: IO ()
main =
  do c <- getContents
     case parse csvFile "(stdin)" c of
       Left e -> do putStrLn "Error parsing input:"
                    print e
       Right r -> mapM_ print r
