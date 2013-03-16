module Types where

import Data.Map


type Word = String

type Positions = [Int]

type OccurrenceMap = Map Word Positions

type VocabularyMap = Map Word [(FilePath, Positions)]

type IndexMap = Map Char VocabularyMap
