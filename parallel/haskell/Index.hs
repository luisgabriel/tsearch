module Index ( empty
             , insert
             , buildQueryIndex
             , find ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Array as Array
import Data.Char ( ord )
import Types

empty :: Index
empty = IMap.empty

insert :: (FilePath, Occurrences) -> Index -> Index
insert (file, o) indexMap = Map.foldrWithKey (mergeIndices file) indexMap o

mergeIndices :: FilePath -> Word -> Positions -> Index -> Index
mergeIndices _ [] _ _ = IMap.empty
mergeIndices file word@(c:_) ps indexMap = IMap.insertWith mergeVocabularies (ord c) vocabulary indexMap
    where
        vocabulary = Map.singleton word [(file, ps)]

mergeVocabularies :: Vocabulary -> Vocabulary -> Vocabulary
mergeVocabularies old new = Map.unionWith (++) old new

buildQueryIndex :: Index -> QueryIndex
buildQueryIndex index = Array.accumArray mergeVocabularies Map.empty (ord '0', ord 'z') (IMap.assocs index)

find :: Word -> QueryIndex -> [(FilePath, Positions)]
find [] _ = []
find word@(c:_) queryIndex = Map.findWithDefault [] word vocabulary
    where
        vocabulary =  queryIndex Array.! (ord c)
