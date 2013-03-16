import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import qualified Data.Map as Map
import Types
import Scanner as Scanner
import Lexer as Lexer
import Index as Index

simpleQuery :: Word -> IndexMap -> [(FilePath, Positions)]
simpleQuery word@(c:_) map = Map.findWithDefault [] word vocabulary
    where
        vocabulary = Map.findWithDefault Map.empty c map

main :: IO ()
main = do
    (path:query:_) <- getArgs
    files <- Scanner.getAllFiles path
    contents <- mapM Lexer.processFile files
    let occurrences = map (\(f, c) -> (f, Lexer.processContent c)) contents
    let indexMap = Index.build occurrences
    let result = simpleQuery query indexMap

    forM_ result $ \(filePath, positions) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show $ length positions)
