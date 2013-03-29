import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent.STM
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )

import Scanner
import Query
import Buffer
import Engine

printResult :: [(FilePath, Int)] -> IO ()
printResult result = do
    let orderedResult = sortBy (mconcat [flip $ comparing snd, comparing fst]) result
    forM_ orderedResult $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)

main :: IO ()
main = do
    (path:rawQuery:_) <- getArgs
    let nWorkers = 8
    let initialSubIndices = 4 :: Int
    let maxFiles = 3 -- max files processed per subindex

    fileBuffer <- atomically newEmptyBuffer
    queryIndexBuffer <- atomically newEmptyBuffer

    Scanner.scan path fileBuffer
    Engine.processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer

    resultVar <- atomically $ newTVar []
    Engine.search (Query.parse rawQuery) queryIndexBuffer resultVar
    finalResult <- atomically $ readTVar resultVar
    printResult finalResult
