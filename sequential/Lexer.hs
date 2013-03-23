module Lexer ( processContent
             , tokenize ) where

import Data.Char ( isAscii, isAlphaNum, toLower )
import qualified Data.Map as Map
import Types

tokenize :: String -> [Word]
tokenize content = words $ map toLower $ filter (\c -> (isAscii c && isAlphaNum c) || c == ' ') content

processContent :: String -> OccurrenceMap
processContent content = process' words 0 Map.empty
    where
        words = tokenize content

process' :: [Word] -> Int -> OccurrenceMap -> OccurrenceMap
process' [] _ map = map
process' (w:ws) i map = process' ws (i + 1) $ Map.insertWith (++) w [i] map
