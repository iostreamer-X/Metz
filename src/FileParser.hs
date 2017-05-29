module FileParser where

import Data.List (isPrefixOf)
import Data.List.Split

import FileSyntax

isComment :: String -> String -> Bool
isComment comment commentStart = commentStart `isPrefixOf` comment

splitToBlocks :: String -> [String]
splitToBlocks = splitOn "\n\n"

lineParser :: String -> String -> Line
lineParser commentStart str = if isComment str commentStart then Comment str else Code str 

blockParser :: String -> [[String]] -> [Block]
blockParser commentStart rawLines = map Block [map (lineParser commentStart) rl |rl <- rawLines ]

parseFile :: String -> FilePath ->  IO [Block]
parseFile commentStart path = blockParser commentStart . map lines . splitToBlocks <$> readFile path

