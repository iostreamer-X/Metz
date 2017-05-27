module CommentLexer where

import Data.List (isPrefixOf)

metzLine :: Char
metzLine = '-'

isComment :: String -> String -> Bool
isComment comment commentStart = commentStart `isPrefixOf` comment

metzBlocks :: String -> [String]
metzBlocks [] = []
metzBlocks (h:str)
  |h == metzLine = getBlocks
  |otherwise     = metzBlocks str
  where
    getBlocks = case metzBlock' [] str of
                  Just (block, rest) -> block : metzBlocks rest
                  Nothing            -> [] 

metzBlock' :: String -> String -> Maybe (String,String)
metzBlock' _ [] = Nothing
metzBlock' bucket (h':str')
  |h' == metzLine = Just (bucket,str')
  |otherwise      = metzBlock' (bucket++[h']) str'