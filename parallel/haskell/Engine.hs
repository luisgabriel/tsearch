module Engine ( processFiles
              , search) where

import Control.Monad ( forM_ )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq

import Lexer
import Index
import Query
import Types
import Buffer

-- (max files per index, TChan (remaing slots, index))
type IndexBuffer = (Int, TChan (Int, Index))

createQueryIndex :: Index -> Buffer QueryIndex -> IO ()
createQueryIndex index buffer = do
    queryIndex <- return $!! Index.buildQueryIndex index
    putStrLn "Sub-index created"
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
    (_, occurrences) <- return $!! Lexer.processContent content

    (fileCounter, index) <- atomically $ readTChan indexBuffer
    let newIndex = Index.insert (filePath, occurrences) index

    putStrLn ("Thread " ++ (show taskId) ++ ". File: " ++ filePath)

    if (fileCounter - 1 > 0)
        then atomically $ writeTChan indexBuffer (fileCounter - 1, newIndex)
        else do
            _ <- forkIO $ createQueryIndex newIndex queryIndexBuffer
            atomically $ writeTChan indexBuffer (maxFiles, Index.empty)


processFile :: Int -> Buffer FilePath -> IndexBuffer -> Buffer QueryIndex -> SSem -> IO ()
processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing = do
    next <- atomically $ readBuffer fileBuffer
    case next of
        Nothing -> atomically $ Sem.signal finishProcessing
        Just filePath -> do
            processFile' taskId filePath (maxFiles, indexBuffer) queryIndexBuffer
            processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing


processFiles :: Int -> Int -> Int -> Buffer FilePath -> Buffer QueryIndex -> IO ()
processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer = do
    indexBuffer <- atomically newTChan
    forM_ [1..initialSubIndices] $ \_ ->
        atomically (writeTChan indexBuffer (maxFiles, Index.empty))

    finishProcessing <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $ \taskId ->
        forkIO $ processFile taskId fileBuffer (maxFiles, indexBuffer) queryIndexBuffer finishProcessing

    _ <- forkIO $ waiter finishProcessing indexBuffer queryIndexBuffer
    return ()


search' :: Query -> QueryIndex -> TVar QueryResult -> IO ()
search' query index resultVar= do
    result <- return $!! Query.perform query index
    putStrLn ("Query \"" ++ (show query) ++ "\" performed")

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
