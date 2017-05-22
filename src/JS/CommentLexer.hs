module JS.CommentLexer where

import Text.Parsec.String (Parser)
import Text.Parsec (many,many1,string,oneOf,noneOf,anyChar)
import Control.Applicative ((<|>))
-- import qualified Data.Text

commentLine :: String
commentLine = "//"

commentLineEnd :: String
commentLineEnd = ".,"

metzLine :: Char
metzLine = '-'

checkForComment :: Parser String
checkForComment = string commentLine

metzBlock :: String -> Maybe String
metzBlock [] = Nothing
metzBlock (x:[]) = Nothing
metzBlock (h:str)
  |h == metzLine = metzBlock' [] str
  |True          = metzBlock str

metzBlock' :: String -> String -> Maybe String
metzBlock' bucket [] = Nothing
metzBlock' bucket (h':str')
  |h' == metzLine = Just bucket
  |True           = metzBlock' (bucket++[h']) str'  