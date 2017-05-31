module FileParser where

import Text.Parsec.Error (ParseError)
import Data.List (isPrefixOf,partition)
import Data.List.Split

import FileSyntax
import qualified Syntax
import CommentParser

isComment :: String -> String -> Bool
isComment commentStart comment = commentStart `isPrefixOf` comment

splitToBlocks :: String -> [String]
splitToBlocks = splitOn "\n\n"

blockParser :: [[String]] -> [Block a]
blockParser rawLines = map Block [map Line $ filter (not.null) rl |rl <- rawLines, not $ null rl ]

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

parseBlock :: Block Comment -> [Either ParseError [Syntax.Expression]]
parseBlock = map parseComment . getBlock

parseBlockPair :: (Block Comment, Block Code) -> ([Either ParseError [Syntax.Expression]], Block Code)
parseBlockPair (commentBlock, codeBlock) = (parseBlock commentBlock, codeBlock)

parseFile :: String -> FilePath -> IO [([Either ParseError [Syntax.Expression]], Block Code)]
parseFile commentStart path = filter notEmpty . map parseBlockPair . partitionBlocks commentStart <$> getBlocksFromFile path
  where
    notEmpty (exprs, _) = not $ all (== Right []) exprs
    getBlocksFromFile = fmap (blockParser . map lines . splitToBlocks) . readFile