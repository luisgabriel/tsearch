import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan
import qualified Data.Map as Map

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

    buffer <- atomically newTChan

    forM_ [1..8] $ \taskId -> forkIO (consumer taskId buffer)

    Scanner.scan path buffer


    threadDelay 4000000

