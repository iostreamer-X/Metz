module FileSyntax where

newtype Block a = Block { getBlock :: [Line a] } deriving (Show)

newtype Line a = Line String deriving (Show)
data Code
data Comment