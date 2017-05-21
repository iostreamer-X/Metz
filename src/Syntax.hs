module Syntax where

data Block = Block deriving (Show)

data Expression
  = Input String | Output String | Annotation Block
    deriving (Show)

isAnnotation :: Expression -> Bool
isAnnotation (Annotation _) = True
isAnnotation _ = False

isExpression :: Expression -> Bool
isExpression = not.isAnnotation