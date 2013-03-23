module Scanner ( scan ) where

import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath
import Control.Monad ( filterM, forM_ )
import Control.Concurrent.STM ( atomically )
import Control.Concurrent.STM.TMVar
import Data.Sequence as Seq

getDirectoryEntries :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryEntries path = do
    entries <- getDirectoryContents path
    let filtered = [path </> e | e <- entries, e `notElem` [".", ".."]]
    files <- filterM doesFileExist filtered
    dirs <- filterM doesDirectoryExist filtered
    return (files, dirs)

scan :: FilePath -> TMVar (Seq FilePath) -> IO ()
scan dirPath bufferVar = do
    (files, dirs) <- getDirectoryEntries dirPath

    atomically $ do
        buffer <- takeTMVar bufferVar
        putTMVar bufferVar $ buffer >< (Seq.fromList files)

    forM_ dirs $ flip scan bufferVar
