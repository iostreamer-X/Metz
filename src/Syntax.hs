module Syntax where

data Block = Block deriving (Show,Eq)

data Expression
  = Input String | Output String | Annotation Block
    deriving (Show,Eq)

isAnnotation :: Expression -> Bool
isAnnotation (Annotation _) = True
isAnnotation _ = False

isExpression :: Expression -> Bool
isExpression = not.isAnnotation