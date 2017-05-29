module FileSyntax where

newtype Block = Block { getBlock :: [Line] } deriving (Eq, Show)
data Line = Code String | Comment String deriving (Eq, Show)