import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.SSem as Sem
import Control.Exception

import Scanner
import Lexer

consumer :: Int -> TChan FilePath -> IO ()
consumer taskId buffer = do
    filePath <- atomically $ readTChan buffer
    content <- readFile filePath
    let occurrenceMap = Lexer.processContent content
    putStrLn $ "Thread " ++ (show taskId) ++ ". File: " ++ filePath ++ ". Words: " ++ (show $ Map.size occurrenceMap)
    consumer taskId buffer


main :: IO ()
main = do
    (path:_) <- getArgs
    let nWorkers = 8;

    buffer <- atomically newTChan

    finishWork <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $
        \taskId -> forkFinally (consumer taskId buffer) (\_ -> (atomically $ Sem.signal finishWork))

    Scanner.scan path buffer

    (atomically $ Sem.wait finishWork)
        `catch` \BlockedIndefinitelyOnSTM -> (atomically $ Sem.wait finishWork)
