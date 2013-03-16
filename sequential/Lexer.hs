module Lexer ( processContent ) where

import Data.Char ( isAlphaNum, toLower )
import qualified Data.Map as Map
import Types

split :: String -> [Word]
split content = words $ map toLower $ filter (\c -> isAlphaNum c || c == ' ') content

processContent :: String -> OccurrenceMap
processContent content = process' words 0 Map.empty
    where
        words = split content

process' :: [Word] -> Int -> OccurrenceMap -> OccurrenceMap
process' [] _ map = map
process' (w:ws) i map = process' ws (i + 1) $ Map.insertWith (++) w [i] map
