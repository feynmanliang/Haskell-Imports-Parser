{-# OPTIONS -Wall -fwarn-tabs #-}
module URLEncodedQueryStringParser where

import Numeric (readHex)
import Data.Functor.Identity (Identity)
import Text.Parsec.Prim (ParsecT)
import Text.ParserCombinators.Parsec

p_query :: CharParser () [(String, Maybe String)]
p_query = p_pair `sepBy` char '&'

p_pair :: ParsecT String () Identity (String, Maybe String)
p_pair = do key <- many1 p_char
            value <- optionMaybe (char '=' >> many p_char)
            return (key, value)

p_char :: ParsecT String () Identity Char
p_char = oneOf urlBaseChars
        <|> (char '+' >> return ' ')
        <|> p_hex

urlBaseChars :: [Char]
urlBaseChars = ['a'..'z']++['A'..'Z']++['0'..'9']++"$-_.!*'(),"

p_hex :: CharParser () Char
p_hex = do
  _ <- char '%'
  a <- hexDigit
  b <- hexDigit
  let ((d, _):_) = readHex [a,b]
  return . toEnum $ d

parseQuery :: String -> Either ParseError [(String, Maybe String)]
parseQuery = parse p_query "(unknown)"
