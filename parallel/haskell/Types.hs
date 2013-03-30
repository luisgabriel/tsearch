module Types where

import Data.Map
import Data.IntMap
import Data.Array

type Word = String

type Positions = [Int]

type Occurrences = Map Word Positions

type Vocabulary = Map Word [(FilePath, Positions)]

data Index = Index Int (IntMap Vocabulary)

data QueryIndex = QueryIndex Int (Array Int Vocabulary)

type QueryResult =  [(FilePath, Int)]


class TSearchIndex a where
    numberOfFiles :: a -> Int

instance TSearchIndex Index where
    numberOfFiles (Index n _) = n

instance TSearchIndex QueryIndex where
    numberOfFiles (QueryIndex n _) = n
