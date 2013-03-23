import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM
import Data.Sequence as Seq
import qualified Data.Map as Map

import Scanner
import Lexer

nextFile :: TMVar (Seq FilePath) -> STM FilePath
nextFile bufferVar = do
    buffer <- takeTMVar bufferVar
    if Seq.null buffer then retry else do
        let (file :< newBuffer) = Seq.viewl buffer
        putTMVar bufferVar newBuffer
        return file

consumer :: Int -> TMVar (Seq FilePath) -> IO ()
consumer tId bufferVar = do
    filePath <- atomically $ nextFile bufferVar
    content <- readFile filePath
    let occurrenceMap = Lexer.processContent content
    putStrLn $ "Thread " ++ (show tId) ++ ". File: " ++ filePath ++ ". Words: " ++ (show $ Map.size occurrenceMap)
    consumer tId bufferVar


main :: IO ()
main = do
    (path:_) <- getArgs

    seqVar <- atomically $ newTMVar Seq.empty

    forM_ [1..8] $ \tId -> forkIO (consumer tId seqVar)

    Scanner.scan path seqVar


    threadDelay 4000000

