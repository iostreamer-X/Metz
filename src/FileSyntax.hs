{-# LANGUAGE ExistentialQuantification #-}

module FileSyntax where

data LineWrapper = forall s.(Line s, Show s) => LineWrapper s
instance Show LineWrapper where
  show (LineWrapper a) = show a

newtype Block = Block { getBlock :: [LineWrapper] } deriving (Show)

newtype Code = Code { getCode :: String } deriving (Eq, Show)
newtype Comment = Comment { getComment :: String } deriving (Eq, Show)

class Line a 
instance Line Code
instance Line Comment