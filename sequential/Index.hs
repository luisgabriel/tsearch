module Index ( build
             , find ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import qualified Data.Array as Array
import Data.Char ( ord )
import Types

type TempIndexMap = IMap.IntMap VocabularyMap

build :: [(FilePath, OccurrenceMap)] -> IndexMap
build occurrences = Array.array (ord '0', ord 'z') (IMap.assocs (build' occurrences IMap.empty))

build' :: [(FilePath, OccurrenceMap)] -> TempIndexMap -> TempIndexMap
build' [] indexMap = indexMap
build' ((file, o):os) indexMap = build' os $ Map.foldrWithKey merge indexMap o
    where
        merge :: Word -> Positions -> TempIndexMap -> TempIndexMap
        merge word@(c:_) ps indexMap = IMap.insertWith mergeVocabularies (ord c) newVocabulary indexMap
            where
                newVocabulary = Map.singleton word [(file, ps)]

mergeVocabularies :: VocabularyMap -> VocabularyMap -> VocabularyMap
mergeVocabularies old new = Map.unionWith (++) old new

find :: Word -> IndexMap -> [(FilePath, Positions)]
find word@(c:_) map = Map.findWithDefault [] word vocabulary
    where
        vocabulary =  map Array.! (ord c)
