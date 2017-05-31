module FileParser where

import Data.List (isPrefixOf,partition)
import Data.List.Split

import FileSyntax

isComment :: String -> String -> Bool
isComment commentStart comment = commentStart `isPrefixOf` comment

splitToBlocks :: String -> [String]
splitToBlocks = splitOn "\n\n"

blockParser :: [[String]] -> [Block a]
blockParser rawLines = map Block [map Line $ filter (not.null) rl |rl <- rawLines, not $ null rl ]

parseFile :: FilePath ->  IO [Block a]
parseFile path = blockParser . map lines . splitToBlocks <$> readFile path

partitionBlocks :: String -> [Block t] -> [(Block Comment, Block Code)]
partitionBlocks commentStart = filter validator . map part
  where
    part = uncurry mapLines . partition(\(Line l)-> isComment commentStart l) . getBlock
    mapLines l1 l2 = (Block $ map makeComment l1, Block $ map makeCode l2)
    makeComment (Line comm) = Line comm :: Line Comment
    makeCode (Line code) = Line code :: Line Code
    validator (Block [], _) = False
    validator (_, Block []) = False
    validator _             = True

