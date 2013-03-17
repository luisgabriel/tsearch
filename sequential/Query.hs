module Query ( simple ) where

import Types
import Index as Index

simple :: Word -> IndexMap -> [(FilePath, Int)]
simple w m = map (\(path, positions) -> (path, length positions)) result
    where
        result = Index.find w m

