module CommentParser where

import Text.Parsec.Error (ParseError,newErrorMessage,Message(Expect))
import Text.Parsec.Pos (initialPos)

import CommentLexer
import Parser
import Syntax
import FileSyntax

parseComment :: Line Comment -> Either ParseError [Expression]
parseComment (Line comment) = validate $ concat <$> sequence blocks
  where
    blocks = map parse $ metzBlocks comment
    validate ex@(Right []) = ex
    validate ex@(Right expressions) = if any isExpression expressions then ex else justAnnotationError comment
    validate err = err
    justAnnotationError comm = Left $ newErrorMessage (Expect "an expression along with the annotation") (initialPos comm)