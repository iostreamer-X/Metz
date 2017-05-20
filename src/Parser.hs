module Parser where

import Text.Parsec (try)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Expr as Ex

import Lexer
import Syntax

import Control.Applicative ((<$>), (<|>))

inputParser :: Parser Expression
inputParser = Input <$> input

outputParser :: Parser Expression
outputParser = Output <$> output

blockParser :: Parser Expression
blockParser = block >>= \_ -> return $ Annotation Block

factor :: Parser Expression
factor = 
  try inputParser  <|>
  try outputParser <|>
  try blockParser

expr :: Parser Expression
expr = Ex.buildExpressionParser [] factor

expressionParser :: Parser [Expression]
expressionParser = commaSep1 expr