module Types where

import Data.Map
import Data.IntMap
import Data.Array

type Word = String

type Positions = [Int]

type Occurrences = Map Word Positions

type Vocabulary = Map Word [(FilePath, Positions)]

type Index = IntMap Vocabulary

type QueryIndex = Array Int Vocabulary

type QueryResult =  [(FilePath, Int)]
