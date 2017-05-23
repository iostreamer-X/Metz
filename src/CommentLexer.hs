module CommentLexer where

metzLine :: Char
metzLine = '-'

metzBlock :: String -> [String]
metzBlock [] = []
metzBlock (h:str)
  |h == metzLine = getBlocks
  |otherwise     = metzBlock str
  where
    getBlocks = case metzBlock' [] str of
                  Just (block, rest) -> block : metzBlock rest
                  Nothing            -> [] 

metzBlock' :: String -> String -> Maybe (String,String)
metzBlock' _ [] = Nothing
metzBlock' bucket (h':str')
  |h' == metzLine = Just (bucket,str')
  |otherwise      = metzBlock' (bucket++[h']) str'  