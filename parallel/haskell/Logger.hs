module Logger ( LogBuffer
              , Logger.log
              , fileProcessed
              , subIndexCompleted
              , queryPerformed
              , searchPerformed
              , listen ) where

import System.IO
import Text.Printf
import Control.Monad ( forM_ )
import Control.Concurrent.STM ( atomically )
import Data.List ( sortBy )
import Data.Monoid ( mconcat )
import Data.Ord ( comparing )

import Query
import Buffer

data Event = FileProcessed Int FilePath Int
           | SubIndexCompleted
           | QueryPerformed Query
           | SearchPerformed [(FilePath, Int)]
           | Message String

type LogBuffer = Buffer Event

-- (kbytes, files, words)
type InfoState = (Double, Int, Int)

fileProcessed :: Buffer Event -> Int -> FilePath -> Int -> IO ()
fileProcessed buffer taskId path words' =  atomically $ writeBuffer buffer $ FileProcessed taskId path words'

subIndexCompleted :: Buffer Event -> IO ()
subIndexCompleted buffer = atomically $ writeBuffer buffer $ SubIndexCompleted

queryPerformed :: Buffer Event -> Query -> IO ()
queryPerformed buffer query = atomically $ writeBuffer buffer $ QueryPerformed query

searchPerformed :: Buffer Event -> [(FilePath, Int)] -> IO ()
searchPerformed buffer result = atomically $ writeBuffer buffer $ SearchPerformed result

log :: Buffer Event -> String -> IO ()
log buffer message = atomically $ writeBuffer buffer $ Message message

listen :: Buffer Event -> IO ()
listen buffer = listen' buffer (0.0, 0, 0)

listen' :: Buffer Event -> InfoState -> IO ()
listen' buffer state = do
    response <- atomically $ readBuffer buffer
    case response of
        Nothing -> return ()
        Just event -> do
            newState <- processEvent event state
            listen' buffer newState

printProgress :: Int -> FilePath -> InfoState -> IO ()
printProgress taskId path (bytes, totalFiles, totalWords) = do
    putStrLn "-----"
    putStrLn $ "[Thread " ++ (show taskId) ++ "]"
    putStrLn $ "New file processed: " ++ path
    putStrLn $ printf "Kbytes processed so far: %.3f" bytes
    putStrLn $ "Files processed so far: " ++ (show totalFiles)
    putStrLn $ "Words found so far: " ++ (show totalWords)

processEvent :: Event -> InfoState -> IO InfoState
processEvent (FileProcessed taskId path words') (bytes, totalFiles, totalWords) = do
    size <- withFile path ReadMode hFileSize
    let newSize = bytes + (fromIntegral size) / 1024
    let newState = (newSize, totalFiles + 1, totalWords + words')
    printProgress taskId path newState
    return newState

processEvent SubIndexCompleted state = do
    putStrLn "-----"
    putStrLn "Sub-index completed"
    return state

processEvent (QueryPerformed query) state = do
    putStrLn "-----"
    putStrLn ("Query \"" ++ (show query) ++ "\" performed")
    return state

processEvent (SearchPerformed result) state = do
    let orderedResult = sortBy (mconcat [flip $ comparing snd, comparing fst]) result
    putStrLn "-----"
    putStrLn "RESULT:"
    forM_ orderedResult $ \(filePath, total) ->
        putStrLn $ "File: " ++ filePath ++ ".   Occurrences: " ++ (show total)
    return state

processEvent (Message msg) state = do
    putStrLn "-----"
    putStrLn msg
    return state
