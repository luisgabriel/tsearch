module Index ( build ) where

import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Data.Char ( ord )
import Types

build :: [(FilePath, OccurrenceMap)] -> IndexMap
build occurrences = build' occurrences IMap.empty

build' :: [(FilePath, OccurrenceMap)] -> IndexMap -> IndexMap
build' [] indexMap = indexMap
build' ((file, o):os) indexMap = build' os $ Map.foldrWithKey merge indexMap o
    where
        merge :: Word -> Positions -> IndexMap -> IndexMap
        merge word@(c:_) ps indexMap = IMap.insertWith mergeVocabularies (ord c) newVocabulary indexMap
            where
                newVocabulary = Map.singleton word [(file, ps)]

mergeVocabularies :: VocabularyMap -> VocabularyMap -> VocabularyMap
mergeVocabularies old new = Map.unionWith (++) old new
