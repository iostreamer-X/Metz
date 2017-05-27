module CommentParser where

import Text.Parsec.Error (ParseError,newErrorMessage,Message(Expect))
import Text.Parsec.Pos (initialPos)

import CommentLexer
import Parser
import Syntax

parseComment :: String -> String -> Either ParseError [Expression]
parseComment code commentStart = validate $ concat <$> sequence blocks
  where
    blocks = map parse . metzBlocks $ if isComment code commentStart then code else []
    validate ex@(Right []) = ex
    validate ex@(Right expressions) = if any isExpression expressions then ex else justAnnotationError code
    validate err = err
    justAnnotationError comm = Left $ newErrorMessage (Expect "an expression along with the annotation") (initialPos comm)