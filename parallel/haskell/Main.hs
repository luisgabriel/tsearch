import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )

import Scanner
import Lexer
import Index
import Query
import Types
import Buffer

-- (max files per index, TChan (remaing slots, index))
type IndexBuffer = (Int, TChan (Int, Index))

createQueryIndex :: Index -> Buffer QueryIndex -> IO ()
createQueryIndex index buffer = do
    let queryIndex = Index.buildQueryIndex index
    let output' = deepseq queryIndex "Query index created"
    putStrLn output'
    atomically $ writeBuffer buffer queryIndex

processRemaingIndices :: TChan (Int, Index) -> Buffer QueryIndex -> IO ()
processRemaingIndices indexBuffer queryIndexBuffer = do
    response <- atomically $ tryReadTChan indexBuffer
    case response of
        Nothing -> atomically $ enableFlag queryIndexBuffer
        Just (_, index) -> do
            createQueryIndex index queryIndexBuffer
            processRemaingIndices indexBuffer queryIndexBuffer

waiter :: SSem -> TChan (Int, Index) -> Buffer QueryIndex -> IO ()
waiter finishProcessing indexBuffer queryIndexBuffer = do
    atomically $ Sem.wait finishProcessing
    processRemaingIndices indexBuffer queryIndexBuffer

processFile' :: Int -> FilePath -> IndexBuffer -> Buffer QueryIndex -> IO ()
processFile' taskId filePath (maxFiles, indexBuffer) queryIndexBuffer = do
    content <- readFile filePath
    let occurrenceMap = Lexer.processContent content

    (fileCounter, index) <- atomically $ readTChan indexBuffer
    let newIndex = Index.insert (filePath, occurrenceMap) index

    let output = deepseq newIndex ("Thread " ++ (show taskId) ++ ". File: " ++ filePath)
    putStrLn output

    if (fileCounter - 1 > 0)
        then atomically $ writeTChan indexBuffer (fileCounter - 1, newIndex)
        else do
            createQueryIndex newIndex queryIndexBuffer
            atomically $ writeTChan indexBuffer (maxFiles, Index.empty)

processFile :: Int -> Buffer FilePath -> IndexBuffer -> Buffer QueryIndex -> SSem -> IO ()
processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing = do
    next <- atomically $ readBuffer fileBuffer
    case next of
        Nothing -> atomically $ Sem.signal finishProcessing
        Just filePath -> do
            processFile' taskId filePath (maxFiles, indexBuffer) queryIndexBuffer
            processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing

search' :: Query -> QueryIndex -> TVar QueryResult -> IO ()
search' query index resultVar= do
    let result = Query.perform query index

    let output = deepseq result ("Query \"" ++ (show query) ++ "\" performed")
    putStrLn output

    atomically $ do
        allResults <- readTVar resultVar
        let newResult = allResults ++ result
        writeTVar resultVar newResult

search :: Query -> Buffer QueryIndex -> TVar QueryResult -> IO ()
search query indexBuffer resultVar = do
    response <- atomically $ readBuffer indexBuffer
    case response of
        Nothing -> return ()
        Just index -> do
            search' query index resultVar
            search query indexBuffer resultVar

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
    indexBuffer <- atomically newTChan
    queryIndexBuffer <- atomically newEmptyBuffer

    forM_ [1..initialSubIndices] $ \_ ->
        atomically (writeTChan indexBuffer (maxFiles, Index.empty))

    Scanner.scan path fileBuffer

    finishProcessing <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $ \taskId ->
        forkIO $ processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing

    _ <- forkIO $ waiter finishProcessing indexBuffer queryIndexBuffer

    resultVar <- atomically $ newTVar []
    search (Query.parse rawQuery) queryIndexBuffer resultVar
    finalResult <- atomically $ readTVar resultVar
    printResult finalResult
