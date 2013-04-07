import System.Environment ( getArgs )
import Control.Monad ( forM, forM_ )
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )

import Types
import Scanner as Scanner
import Lexer as Lexer
import Index as Index
import Query as Query

printResult :: [(FilePath, Int)] -> IO ()
printResult result = do
    let orderedResult = sortBy (mconcat [flip $ comparing snd, comparing fst]) result
    forM_ orderedResult $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)

main :: IO ()
main = do
    (path:queryString:_) <- getArgs
    files <- Scanner.getAllFiles path

    occurrences <- forM files $ \path -> do
        content <- readFile path
        let occurrenceMap = Lexer.processContent content
        return (path, occurrenceMap)

    let indexMap = Index.build occurrences
    let query = Query.parse queryString
    let result = Query.perform query indexMap
    printResult result
