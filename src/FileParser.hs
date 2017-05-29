module FileParser where

import Data.List (isPrefixOf)
import Data.List.Split

import FileSyntax

isComment :: String -> String -> Bool
isComment comment commentStart = commentStart `isPrefixOf` comment

splitToBlocks :: String -> [String]
splitToBlocks = splitOn "\n\n"

blockParser :: [[String]] -> [Block a]
blockParser rawLines = map Block [map Line rl |rl <- rawLines ]

parseFile :: FilePath ->  IO [Block a]
parseFile path = blockParser . map lines . splitToBlocks <$> readFile path

