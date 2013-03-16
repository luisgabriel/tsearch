import System.Environment ( getArgs )
import Control.Monad ( forM, forM_ )
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )
import Types
import Scanner as Scanner
import Lexer as Lexer
import Index as Index

simpleQuery :: Word -> IndexMap -> [(FilePath, Int)]
simpleQuery w m = map (\(path, positions) -> (path, length positions)) result
    where
        result = Index.find w m

printResult :: [(FilePath, Int)] -> IO ()
printResult result = do
    let orderedResult = sortBy (mconcat [flip $ comparing snd, comparing fst]) result
    forM_ orderedResult $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)

main :: IO ()
main = do
    (path:query:_) <- getArgs
    files <- Scanner.getAllFiles path

    occurrences <- forM files $ \path -> do
        content <- readFile path
        let occurrenceMap = Lexer.processContent content
        return (path, occurrenceMap)

    let indexMap = Index.build occurrences
    let result = simpleQuery query indexMap
    printResult result
