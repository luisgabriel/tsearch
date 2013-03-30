import System.Environment ( getArgs )
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM

import Scanner
import Query
import Buffer
import Engine
import Logger ( listen )

main :: IO ()
main = do
    (path:rawQuery:_) <- getArgs
    let nWorkers = 8
    let initialSubIndices = 4 :: Int
    let maxFiles = 3 -- max files processed per subindex

    logBuffer <- atomically newEmptyBuffer
    _ <- forkIO $ Logger.listen logBuffer

    fileBuffer <- atomically newEmptyBuffer
    _ <- forkIO $ Scanner.scan path fileBuffer

    queryIndexBuffer <- atomically newEmptyBuffer
    Engine.processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer

    Engine.search (Query.parse rawQuery) queryIndexBuffer [] logBuffer
    threadDelay 10000
