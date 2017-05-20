module Lexer where

import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token

import Control.Applicative ((<*))

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    names = ["is","block","input","output"]
    style = emptyDef {
               Token.reservedNames = names
             }
reserved :: String -> Parser ()
reserved = Token.reserved lexer

identifier :: Parser String
identifier = Token.identifier lexer

commaSep1 :: Parser a -> Parser [a]
commaSep1 = Token.commaSep1 lexer

input :: Parser String
input = identifier <* (reserved "is" >> reserved "input")

output :: Parser String
output = identifier <* (reserved "is" >> reserved "output")

block :: Parser ()
block = reserved "block"