import System.Environment ( getArgs )
import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath
import Control.Monad ( filterM, forM_ )
import Data.Char ( isAlphaNum, toLower )
import qualified Data.Map as Map

-- Begin Module
getDirectoryEntries :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryEntries path = do
    entries <- getDirectoryContents path
    let filtered = [path </> e | e <- entries, e `notElem` [".", ".."]]
    files <- filterM doesFileExist filtered
    dirs <- filterM doesDirectoryExist filtered
    return (files, dirs)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dirPath = do
    (files, dirs) <- getDirectoryEntries dirPath
    list <- mapM getAllFiles dirs
    let allFiles = foldr (++) files list
    return allFiles

processFile :: FilePath -> IO (FilePath, [String])
processFile filePath = do
    contents <- readFile filePath
    let allWords = words $ map toLower $ filter (\c -> isAlphaNum c || c == ' ') contents
    return (filePath, allWords)
-- End Module

type Word = String

type Positions = [Int]

type OccurrenceMap = Map.Map Word Positions

type VocabularyMap = Map.Map Word [(FilePath, Positions)]

type IndexMap = Map.Map Char VocabularyMap

processContent :: [Word] -> OccurrenceMap
processContent words = process' words 0 Map.empty
    where
        process' [] _ map = map
        process' (w:ws) i map = process' ws (i + 1) $ Map.insertWith (++) w [i] map

buildIndex :: [(FilePath, OccurrenceMap)] -> IndexMap
buildIndex occurrences = build' occurrences Map.empty

build' :: [(FilePath, OccurrenceMap)] -> IndexMap -> IndexMap
build' [] indexMap = indexMap
build' ((file, o):os) indexMap = build' os $ Map.foldrWithKey merge indexMap o
    where
        merge :: Word -> Positions -> IndexMap -> IndexMap
        merge word@(c:_) ps indexMap = Map.insertWith mergeVocabularies c newVocabulary indexMap
            where
                newVocabulary = Map.singleton word [(file, ps)]

        mergeVocabularies :: VocabularyMap -> VocabularyMap -> VocabularyMap
        mergeVocabularies old new = Map.unionWith (++) old new

simpleQuery :: Word -> IndexMap -> [(FilePath, Positions)]
simpleQuery word@(c:_) map = Map.findWithDefault [] word vocabulary
    where
        vocabulary = Map.findWithDefault Map.empty c map

main :: IO ()
main = do
    (path:query:_) <- getArgs
    files <- getAllFiles path
    contents <- mapM processFile files
    let occurrences = map (\(f, c) -> (f, processContent c)) contents
    let indexMap = buildIndex occurrences
    let result = simpleQuery query indexMap

    forM_ result $ \(filePath, positions) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show $ length positions)
