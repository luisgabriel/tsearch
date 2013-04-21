import System.Environment ( getArgs )
import Control.Concurrent ( forkIO, forkFinally )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM.SSem as Sem

import Scanner
import Buffer
import Engine
import Logger

main :: IO ()
main = do
    (path:nW:rawQueries) <- getArgs
    let nWorkers = (read nW) :: Int
    let initialSubIndices = 4 :: Int
    let maxFiles = 3 -- max files processed per subindex

    loggerFinished <- atomically $ newEmptyTMVar
    logBuffer <- atomically newEmptyBuffer
    _ <- forkFinally (Logger.listen logBuffer) (\_ -> atomically $ putTMVar loggerFinished ())

    fileBuffer <- atomically newEmptyBuffer
    _ <- forkIO $ Scanner.scan path fileBuffer

    queryIndexBuffer <- atomically newEmptyBuffer
    Engine.processFiles initialSubIndices maxFiles nWorkers fileBuffer queryIndexBuffer logBuffer

    finishSearch <- Engine.processSearch rawQueries queryIndexBuffer logBuffer

    atomically $ Sem.wait finishSearch
    Logger.finish logBuffer
    atomically $ takeTMVar loggerFinished
