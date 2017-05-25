module CommentParser where

import Text.Parsec.Error (ParseError,newErrorMessage,Message(Expect))
import Text.Parsec.Pos (initialPos)

import CommentLexer
import Parser
import Syntax

parseComment :: String -> Either ParseError [Expression]
parseComment comment = validate $ concat <$> sequence blocks
  where
    blocks = map parse . metzBlocks $ comment
    validate ex@(Right expressions) = if any isExpression expressions then ex else noExpressionError comment
    validate err = err
    noExpressionError comm = Left $ newErrorMessage (Expect "an expression") (initialPos comm)