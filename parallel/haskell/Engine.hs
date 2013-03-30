module Engine ( processFiles
              , search) where

import Control.Monad ( forM_ )
import Control.Concurrent ( forkIO )
import Control.Concurrent.STM
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq
import qualified Data.Set as Set
import qualified Data.Map as Map ( keysSet )

import Lexer
import Index
import Query
import Types
import Buffer
import Logger

createQueryIndex :: Index -> Buffer QueryIndex -> LogBuffer -> IO ()
createQueryIndex index buffer logBuffer = do
    let queryIndex = Index.buildQueryIndex index
    atomically $ writeBuffer buffer queryIndex
    Logger.subIndexCompleted logBuffer


processRemaingIndices :: TChan Index -> Buffer QueryIndex -> LogBuffer -> IO ()
processRemaingIndices indexBuffer queryIndexBuffer logBuffer = do
    response <- atomically $ tryReadTChan indexBuffer
    case response of
        Nothing -> atomically $ enableFlag queryIndexBuffer
        Just index -> do
            createQueryIndex index queryIndexBuffer logBuffer
            processRemaingIndices indexBuffer queryIndexBuffer logBuffer


waiter :: SSem -> TChan Index -> Buffer QueryIndex -> LogBuffer -> IO ()
waiter finishProcessing indexBuffer queryIndexBuffer logBuffer = do
    atomically $ Sem.wait finishProcessing
    processRemaingIndices indexBuffer queryIndexBuffer logBuffer


processFile' :: Int -> FilePath -> Int -> TChan Index -> TVar (Set.Set Word) -> Buffer QueryIndex -> LogBuffer -> IO ()
processFile' taskId filePath maxFiles indexBuffer wordSetVar queryIndexBuffer logBuffer = do
    content <- readFile filePath
    (words', occurrences) <- return $!! Lexer.processContent content

    wordSet <- atomically $ readTVar wordSetVar
    let newWordSet = Set.union wordSet $ Map.keysSet occurrences
    atomically $ writeTVar wordSetVar newWordSet

    index <- atomically $ readTChan indexBuffer
    let newIndex = Index.insert (filePath, occurrences) index

    indexedWords' <- return $! Set.size newWordSet
    Logger.fileProcessed logBuffer taskId filePath words' indexedWords'

    if ((numberOfFiles newIndex) < maxFiles)
        then atomically $ writeTChan indexBuffer newIndex
        else do
            _ <- forkIO $ createQueryIndex newIndex queryIndexBuffer logBuffer
            atomically $ writeTChan indexBuffer Index.empty


processFile :: Int -> Buffer FilePath -> Int -> TChan Index -> TVar (Set.Set Word) -> Buffer QueryIndex -> LogBuffer -> SSem -> IO ()
processFile taskId fileBuffer maxFiles indexBuffer wordSetVar queryIndexBuffer logBuffer finishProcessing = do
    next <- atomically $ readBuffer fileBuffer
    case next of
        Nothing -> atomically $ Sem.signal finishProcessing
        Just filePath -> do
            processFile' taskId filePath maxFiles indexBuffer wordSetVar queryIndexBuffer logBuffer
            processFile taskId fileBuffer maxFiles indexBuffer wordSetVar queryIndexBuffer logBuffer finishProcessing


processFiles :: Int -> Int -> Int -> Buffer FilePath -> Buffer QueryIndex -> LogBuffer -> IO ()
processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer = do
    indexBuffer <- atomically newTChan
    forM_ [1..initialSubIndices] $ \_ ->
        atomically (writeTChan indexBuffer Index.empty)

    wordSetVar <- atomically $ newTVar Set.empty

    finishProcessing <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $ \taskId ->
        forkIO $ processFile taskId fileBuffer maxFiles indexBuffer wordSetVar queryIndexBuffer logBuffer finishProcessing

    _ <- forkIO $ waiter finishProcessing indexBuffer queryIndexBuffer logBuffer
    return ()


search' :: Query -> QueryIndex -> QueryResult -> QueryResult
search' query index allResults = allResults ++ (Query.perform query index)

search :: Query -> Buffer QueryIndex -> QueryResult -> SSem -> LogBuffer -> IO ()
search query indexBuffer result finishSearch logBuffer = do
    response <- atomically $ readBuffer indexBuffer
    case response of
        Nothing -> do
            Logger.searchPerformed logBuffer result
            atomically $ Sem.signal finishSearch
        Just index -> do
            newResult <- return $!! search' query index result
            Logger.queryPerformed logBuffer query
            search query indexBuffer newResult finishSearch logBuffer
