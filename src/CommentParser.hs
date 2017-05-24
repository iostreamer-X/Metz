module CommentParser where

import Data.Either

import Text.Parsec.Error (ParseError,newErrorMessage,Message(Expect))
import Text.Parsec.Pos (initialPos)

import CommentLexer
import Parser
import Syntax

parseComment :: String -> Either [ParseError] [Expression]
parseComment comment = validate $ if all isRight blocks then Right $ getExpressions blocks else Left $ getErrors $ filter isLeft blocks
  where
    blocks = map parse . metzBlocks $ comment
    validate ex@(Right expressions) = if any isExpression expressions then ex else noExpressionError comment
    validate er@(Left _) = er
    noExpressionError comm = Left [newErrorMessage (Expect "an expression") (initialPos comm)]

getExpressions :: [Either t [Expression]] -> [Expression]
getExpressions = concatMap (\(Right expr) -> expr) 

getErrors :: [Either ParseError t] -> [ParseError]
getErrors = map (\(Left err) -> err)   