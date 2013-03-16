module Lexer ( processFile
             , processContent ) where

import Data.Char ( isAlphaNum, toLower )
import qualified Data.Map as Map
import Types

processFile :: FilePath -> IO (FilePath, [String])
processFile filePath = do
    contents <- readFile filePath
    let allWords = words $ map toLower $ filter (\c -> isAlphaNum c || c == ' ') contents
    return (filePath, allWords)

processContent :: [Word] -> OccurrenceMap
processContent words = process' words 0 Map.empty
    where
        process' [] _ map = map
        process' (w:ws) i map = process' ws (i + 1) $ Map.insertWith (++) w [i] map
