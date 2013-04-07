module Scanner ( getAllFiles ) where

import System.Directory ( getDirectoryContents, doesFileExist, doesDirectoryExist )
import System.FilePath
import Control.Monad ( filterM )

getDirectoryEntries :: FilePath -> IO ([FilePath], [FilePath])
getDirectoryEntries path = do
    entries <- getDirectoryContents path
    let filtered = [path </> e | e <- entries, e `notElem` [".", ".."]]
    files <- filterM doesFileExist filtered
    dirs <- filterM doesDirectoryExist filtered
    return (files, dirs)

getAllFiles :: FilePath -> IO [FilePath]
getAllFiles dirPath = do
    (files, dirs) <- getDirectoryEntries dirPath
    list <- mapM getAllFiles dirs
    let allFiles = foldr (++) files list
    return allFiles
