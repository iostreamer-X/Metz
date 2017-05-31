module JS.Collect where

import Text.Parsec(noneOf,string,many1,eof)
import Text.Parsec.String(Parser)

import Data.List.Split

delimiter :: String
delimiter = "."

commentStart :: String
commentStart = "//"

stringToParser :: String -> Parser String
stringToParser "_" = many1 $ noneOf delimiter 
stringToParser  s  = string s

inputParser :: String -> Parser String
inputParser str = 
  let patternParsers = mapParser $ init input
      varParser      = stringToParser (last input) <* eof
  in  foldr1 (>>) (patternParsers ++ [varParser])
  where 
    mapParser = map ((<* string delimiter).stringToParser)
    input = splitOn delimiter str