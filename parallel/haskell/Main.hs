import System.Environment ( getArgs )
import Control.Monad ( forM_ )
import Control.Concurrent ( forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.SSem as Sem
import Control.DeepSeq
import Control.Exception

import Scanner
import Lexer
import Index
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


main :: IO ()
main = do
    (path:_) <- getArgs
    let nWorkers = 8
    let nSubIndices = 4 :: Int

    indexBuffer <- atomically newTChan
    forM_ [1..nSubIndices] $ \_ -> atomically (writeTChan indexBuffer Index.empty)

    fileBuffer <- atomically newTChan

    finishWork <- atomically $ Sem.new (1 - nWorkers)
    forM_ [1..nWorkers] $
        \taskId -> forkFinally (processFile taskId fileBuffer indexBuffer) (\_ -> (atomically $ Sem.signal finishWork))

    Scanner.scan path fileBuffer

    (atomically $ Sem.wait finishWork)
        `catch` \BlockedIndefinitelyOnSTM -> (atomically $ Sem.wait finishWork)
