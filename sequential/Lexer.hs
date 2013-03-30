module Lexer ( processContent
             , tokenize ) where

import Data.Char ( isAscii, isAlphaNum, toLower )
import qualified Data.Map as Map
import Types

tokenize :: String -> [Word]
tokenize content = words $ map lowerAndReplace $ filter isValid content
    where
        isValid c = (isAscii c && isAlphaNum c) || c == ' ' || c == '\n'
        lowerAndReplace c = if (c == '\n') then ' ' else toLower c

processContent :: String -> OccurrenceMap
processContent content = process' words' 0 Map.empty
    where
        words' = tokenize content

process' :: [Word] -> Int -> OccurrenceMap -> OccurrenceMap
process' [] _ map' = map'
process' (w:ws) i map' = process' ws (i + 1) $ Map.insertWith (++) w [i] map'
