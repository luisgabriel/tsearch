import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Data.Char ( ord )
import qualified Data.Map as Map
import qualified Data.IntMap as IMap
import Types
import Scanner as Scanner
import Lexer as Lexer
import Index as Index

simpleQuery :: Word -> IndexMap -> [(FilePath, Positions)]
simpleQuery word@(c:_) map = Map.findWithDefault [] word vocabulary
    where
        vocabulary = IMap.findWithDefault Map.empty (ord c) map

printResult :: [(FilePath, Positions)] -> IO ()
printResult result = do
    forM_ result $ \(filePath, positions) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show $ length positions)

main :: IO ()
main = do
    (path:query:_) <- getArgs
    files <- Scanner.getAllFiles path
    contents <- mapM Lexer.processFile files
    let occurrences = map (\(f, c) -> (f, Lexer.processContent c)) contents
    let indexMap = Index.build occurrences
    let result = simpleQuery query indexMap
    printResult result
