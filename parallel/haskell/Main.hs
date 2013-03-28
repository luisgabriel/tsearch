import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkFinally, forkIO )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq
import Control.Exception
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )

import Scanner
import Lexer
import Index
import Query
import Types

processFile :: Int -> TChan FilePath -> TChan Index -> IO ()
processFile taskId fileBuffer indexBuffer = do
    filePath <- atomically $ readTChan fileBuffer
    content <- readFile filePath
    let occurrenceMap = Lexer.processContent content

    index <-  atomically $ readTChan indexBuffer
    let newIndex = Index.insert (filePath, occurrenceMap) index
    atomically $ writeTChan indexBuffer newIndex

    let output = deepseq newIndex ("Thread " ++ (show taskId) ++ ". File: " ++ filePath)
    putStrLn output

    processFile taskId fileBuffer indexBuffer

generateQueryIndices :: TChan Index -> TChan QueryIndex -> IO ()
generateQueryIndices indexBuffer queryIndexBuffer = do
    index <- atomically $ readTChan indexBuffer
    let queryIndex = Index.buildQueryIndex index

    let output = deepseq queryIndex "Query index created"
    putStrLn output

    atomically $ writeTChan queryIndexBuffer queryIndex
    generateQueryIndices indexBuffer queryIndexBuffer

search :: String -> TChan QueryIndex -> TVar QueryResult -> IO ()
search rawQuery indexBuffer resultContainer = do
    let query = Query.parse rawQuery
    index <- atomically $ readTChan indexBuffer
    let result = Query.perform query index

    let output = deepseq result ("Query \"" ++ rawQuery ++ "\" performed")
    putStrLn output

    atomically $ do
        allResults <- readTVar resultContainer
        let newResult = allResults ++ result
        writeTVar resultContainer newResult

printResult :: [(FilePath, Int)] -> IO ()
printResult result = do
    let orderedResult = sortBy (mconcat [flip $ comparing snd, comparing fst]) result
    forM_ orderedResult $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)

main :: IO ()
main = do
    (path:rawQuery:_) <- getArgs
    let nWorkers = 8
    let nSubIndices = 1 :: Int

    indexBuffer <- atomically newTChan
    forM_ [1..nSubIndices] $ \_ ->
        atomically (writeTChan indexBuffer Index.empty)

    fileBuffer <- atomically newTChan

    finishWork <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $ \taskId ->
        forkFinally (processFile taskId fileBuffer indexBuffer) (\_ -> (atomically $ Sem.signal finishWork))

    Scanner.scan path fileBuffer

    (atomically $ Sem.wait finishWork)
        `catch` \BlockedIndefinitelyOnSTM -> (atomically $ Sem.wait finishWork)

    queryIndexBuffer <- atomically newTChan
    _ <- forkIO $ generateQueryIndices indexBuffer queryIndexBuffer


    resultContainer <- atomically $ newTVar []
    search rawQuery queryIndexBuffer resultContainer
    finalResult <- atomically $ readTVar resultContainer
    printResult finalResult
