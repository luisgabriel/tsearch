import System.Environment ( getArgs )
import Control.Monad ( forM_ )
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
    forM_ result $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)

main :: IO ()
main = do
    (path:query:_) <- getArgs
    files <- Scanner.getAllFiles path
    contents <- mapM Lexer.processFile files
    let occurrences = map (\(f, c) -> (f, Lexer.processContent c)) contents
    let indexMap = Index.build occurrences
    let result = simpleQuery query indexMap
    printResult result
