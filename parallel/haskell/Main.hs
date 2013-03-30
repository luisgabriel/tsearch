import System.Environment ( getArgs )
import Control.Concurrent ( forkIO, forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.SSem as Sem

import Scanner
import Query
import Buffer
import Engine
import Logger

main :: IO ()
main = do
    (path:rawQuery:_) <- getArgs
    let nWorkers = 8
    let initialSubIndices = 4 :: Int
    let maxFiles = 3 -- max files processed per subindex

    loggerFinished <- atomically $ newEmptyTMVar
    logBuffer <- atomically newEmptyBuffer
    _ <- forkFinally (Logger.listen logBuffer) (\_ -> atomically $ putTMVar loggerFinished ())

    fileBuffer <- atomically newEmptyBuffer
    _ <- forkIO $ Scanner.scan path fileBuffer

    queryIndexBuffer <- atomically newEmptyBuffer
    Engine.processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer

    finishSearch <- atomically $ Sem.new 0
    Engine.search (Query.parse rawQuery) queryIndexBuffer [] finishSearch logBuffer

    atomically $ Sem.wait finishSearch
    Logger.finish logBuffer
    atomically $ takeTMVar loggerFinished
