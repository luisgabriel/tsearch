module Types where

import Data.Map
import Data.Array


type Word = String

type Positions = [Int]

type OccurrenceMap = Map Word Positions

type VocabularyMap = Map Word [(FilePath, Positions)]

type IndexMap = Array Int VocabularyMap
