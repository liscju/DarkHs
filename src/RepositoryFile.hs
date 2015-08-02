module RepositoryFile where

import Repository

import System.Directory
import Control.Exception
import System.FilePath
import Control.Monad
import System.IO

topdir :: FilePath
topdir = ".darkhs"

headFile :: FilePath
headFile = joinPath [topdir, "HEAD"]

commitsDir :: FilePath
commitsDir = joinPath [topdir, "commits"]

filesDir :: FilePath
filesDir = joinPath [topdir, "files"]

treesDir :: FilePath
treesDir = joinPath [topdir, "trees"]

initialHeadCommitId :: CommitId
initialHeadCommitId = 0

saveCommitToFile :: CommitId -> CommitInfo -> IO ()
saveCommitToFile commitId commitInfo =
    withFile (combine commitsDir (show commitId)) AppendMode $ \handle -> do
                hPutStr handle (show commitInfo)

saveHeadToFile :: CommitId -> IO ()
saveHeadToFile commitId =
    withFile headFile WriteMode (flip hPutStr (show commitId))

saveTreeToFile :: TreeId -> [RepoTreeFile] -> IO ()
saveTreeToFile treeId repoTreeFiles =
    do
        withFile (combine treesDir (show treeId)) AppendMode $ \handle -> do
                    forM repoTreeFiles (hPutStrLn handle . show)
        return ()

getCurrentCommitId :: IO Int
getCurrentCommitId =
    do
        head <- readFile headFile
        return (read head)

generateNextCommitId :: IO Int
generateNextCommitId =
    generateNextId commitsDir

generateNextFileId :: IO Int
generateNextFileId =
    generateNextId filesDir

generateNextTreeId :: IO Int
generateNextTreeId =
    generateNextId treesDir

generateNextId :: FilePath -> IO Int
generateNextId path =
    do
        dirContent <- getDirectoryContents path
        let currentBiggestId = getBiggestId dirContent
        return (currentBiggestId + 1)

getBiggestId :: [FilePath] -> Int
getBiggestId [] = 0
getBiggestId paths
    | ((map read filtered_paths) :: [Int]) == [] = 0
    | otherwise = maximum $ ((map read filtered_paths) :: [Int])
        where filtered_paths = filter (\x -> x /= "." && x /= "..") paths

copyWorkingDirectoryToRepoFiles :: Int -> IO [RepoTreeFile]
copyWorkingDirectoryToRepoFiles firstFileId =
    do
        workingDirFilePaths <- getRecursiveContents "."
        let repoArchivedFilePaths = generateRepoFilesHolders firstFileId (length workingDirFilePaths)
        forM (zip workingDirFilePaths repoArchivedFilePaths) copyFileFromWorkingDirectoryToRepoFile

copyFileFromWorkingDirectoryToRepoFile :: (FilePath,FilePath) -> IO RepoTreeFile
copyFileFromWorkingDirectoryToRepoFile (workingPath,repoPath) =
    do
        isFile <- doesFileExist workingPath
        if isFile
            then do
                copyFile workingPath repoPath
                return  $ RepoFile workingPath ((read (takeFileName repoPath)) :: Int)
            else return $ RepoDir workingPath

generateRepoFilesHolders :: FileId -> Int -> [FilePath]
generateRepoFilesHolders firstFileId count =
    map (combine filesDir) (map show [firstFileId .. (firstFileId + count - 1)])

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
    names <- getDirectoryContents topdir
    let properNames = filter (`notElem` [".", "..", ".darkhs"]) names
    paths <- forM properNames $ \name -> do
        let path = topdir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
            then getRecursiveContents path >>= (\x -> return $ [path] ++ x)
            else return [path]
    return (concat paths)

