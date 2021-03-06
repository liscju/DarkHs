module RepositoryFile where

import Repository
import Diff

import System.Directory
import Control.Exception
import System.FilePath
import Control.Monad
import System.IO
import Data.Maybe

import Data.List

topdir :: FilePath
topdir = ".darkhs"

headFile :: FilePath
headFile = joinPath [topdir, "HEAD"]

currentBranchPointerFile :: FilePath
currentBranchPointerFile = joinPath [branchesDir, "CURRENT"]

masterBranchFile :: FilePath
masterBranchFile = joinPath [branchesDir, "master"]

commitsDir :: FilePath
commitsDir = joinPath [topdir, "commits"]

filesDir :: FilePath
filesDir = joinPath [topdir, "files"]

treesDir :: FilePath
treesDir = joinPath [topdir, "trees"]

branchesDir :: FilePath
branchesDir = joinPath [topdir, "branches"]

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

getCurrentCommitId :: IO CommitId
getCurrentCommitId =
    do
        head <- readFile headFile
        return (read head)

getCommitInformation :: CommitId -> IO CommitInfo
getCommitInformation commitId =
    (readFile $ combine commitsDir (show commitId)) >>= return . read

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

copyWorkingDirectoryToRepoFiles :: IO TreeInfo
copyWorkingDirectoryToRepoFiles =
    do
        firstFileId <- generateNextFileId
        workingDirFilePaths <- getRecursiveContents "."
        let repoArchivedFilePaths = generateRepoFilesHolders firstFileId (length workingDirFilePaths)
        forM (zip workingDirFilePaths repoArchivedFilePaths) copyFileFromWorkingDirectoryToRepoFile

getTreeInfoForCommitId :: CommitId -> IO TreeInfo
getTreeInfoForCommitId commitId =
    getCommitInformation commitId >>=
                    (\commitInfo -> return $ getCommitTreeId commitInfo) >>= getTreeInfo

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

getContentOfRepoFile :: FileId -> IO String
getContentOfRepoFile fileId =
    readFile $ filesDir </> (show fileId)

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

getLogInformation :: CommitId -> IO [Commit]
getLogInformation 0 = return []
getLogInformation commitId =
    do
        commitInformation <- getCommitInformation commitId
        logInformationAboutParents <- getLogInformation (getParentCommit commitInformation)
        return $ [(commitId,commitInformation)] ++ logInformationAboutParents

getTreeInfo :: TreeId -> IO TreeInfo
getTreeInfo treeId =
    do
        treeFileContent <- readFile $ combine treesDir (show treeId)
        let treeInfoLines = lines treeFileContent
        return $ map read treeInfoLines

clearCurrentWorkingDirectory :: IO ()
clearCurrentWorkingDirectory =
    do
        topDirNames <- getDirectoryContents "."
        let properTopDirNames = filter (`notElem` [".", "..", ".darkhs"]) topDirNames
        forM properTopDirNames $ \name -> do
            isDirectory <- doesDirectoryExist name
            if isDirectory
                then removeDirectoryRecursive name
                else removeFile name
        return ()

copyRepoFilesToWorkingDirectory :: TreeInfo -> IO ()
copyRepoFilesToWorkingDirectory treeInfo =
    forM treeInfo copyRepoFileToWorkingDirectory >> return ()

copyRepoFileToWorkingDirectory :: RepoTreeFile -> IO ()
copyRepoFileToWorkingDirectory (RepoDir dirPath) = createDirectory dirPath
copyRepoFileToWorkingDirectory (RepoFile filePath id) =
    copyFile (filesDir </> (show id)) filePath


changeCurrentDirectoryToMainRepoDirectory :: IO ()
changeCurrentDirectoryToMainRepoDirectory =
    do
        currentDirectory <- getCurrentDirectory
        currentRepoMainDir <- findCurrentRepoMainDir currentDirectory
        case currentRepoMainDir of
            Nothing -> error "fatal: Not a darkhs repository (or any of the parents)"
            Just path -> setCurrentDirectory path
    where
        findCurrentRepoMainDir :: FilePath -> IO (Maybe FilePath)
        findCurrentRepoMainDir path =
            do
                if isDrive path
                    then return Nothing
                    else do
                        hasMainRepoDir <- doesDirectoryExist (path </> topdir)
                        if hasMainRepoDir
                            then return $ Just path
                            else findCurrentRepoMainDir $ takeDirectory path

updateCurrentBranchCommit :: CommitId -> IO ()
updateCurrentBranchCommit commitId =
    do
        currentBranchPointer <- getCurrentBranchPointer
        case currentBranchPointer of
            BranchPointer branchId -> changeBranchCommit branchId commitId
            DetachedPointer -> return ()

getCurrentBranchPointer :: IO CurrentBranchPointer
getCurrentBranchPointer =
    readFile currentBranchPointerFile >>= return . read

changeBranchCommit :: BranchId -> CommitId -> IO ()
changeBranchCommit branchId commitId =
    writeFile (branchesDir </> branchId) $ show commitId

getBranchCommit :: BranchId -> IO CommitId
getBranchCommit branchId =
    readFile (branchesDir </> branchId) >>= return . read

createNewBranch :: BranchId -> CommitId -> IO ()
createNewBranch branchId commitId =
    do
        doesBranchExists <- doesFileExist (branchesDir </> branchId)
        if doesBranchExists
            then
                do
                    error $ "Branch " ++ branchId ++ " already exists"
                    return ()
            else changeBranchCommit branchId commitId
        return ()

detachCurrentBranchPointer :: IO ()
detachCurrentBranchPointer =
    writeFile currentBranchPointerFile (show DetachedPointer)

attachCurrentBranchPointer :: BranchId -> IO ()
attachCurrentBranchPointer branchId =
    writeFile currentBranchPointerFile $ show $ BranchPointer branchId

compareTreeInfos :: TreeInfo -> TreeInfo -> IO TreeInfoComparison
compareTreeInfos newTreeInfo oldTreeInfo =
    do
        let oldTreeInfoFilePaths = map getRepoTreeFilePath oldTreeInfo
            newTreeInfoFilePaths = map getRepoTreeFilePath newTreeInfo
            sharedFiles  = intersect oldTreeInfoFilePaths newTreeInfoFilePaths
            addedFiles   = newTreeInfoFilePaths \\ oldTreeInfoFilePaths
            removedFiles = oldTreeInfoFilePaths \\ newTreeInfoFilePaths

        modifiedFiles <-
            flip filterM sharedFiles
                (\path -> hasTreeInfosInGivenPathSameContent newTreeInfo oldTreeInfo path >>= return . not)

        return (modifiedFiles, addedFiles, removedFiles)

isRepoTreeFilesSame :: RepoTreeFile -> RepoTreeFile -> IO Bool
isRepoTreeFilesSame (RepoDir path1) (RepoDir path2) = return (path1 == path2)
isRepoTreeFilesSame (RepoFile path1 fileId1) (RepoFile path2 fileId2) = do
    content1 <- getContentOfRepoFile fileId1
    content2 <- getContentOfRepoFile fileId2
    return (path1 == path2 && content1 == content2)
isRepoTreeFilesSame _ _ = return False

hasTreeInfosInGivenPathSameContent :: TreeInfo -> TreeInfo -> FilePath -> IO Bool
hasTreeInfosInGivenPathSameContent newTreeInfo oldTreeInfo path =
    do
        let
            newFileTreeInfoEl = findRepoFileInTreeInfo newTreeInfo path
            oldFileTreeInfoEl = findRepoFileInTreeInfo oldTreeInfo path
        case (newFileTreeInfoEl,oldFileTreeInfoEl) of
            (Just newEl,Just oldEl) -> (isRepoTreeFilesSame newEl oldEl) >>= return
            otherwise -> return False


diffTreeInfos :: TreeInfo -> TreeInfo -> IO DiffedFileTree
diffTreeInfos newTreeInfo oldTreeInfo =
    do
        let filesTreeComparison = compareTreeInfos newTreeInfo oldTreeInfo
        (modifiedFiles, addedFiles, removedFiles) <- filesTreeComparison

        modifiedDiffFileTree <-
            forM modifiedFiles (diffModifiedTreeInfoFileInGivenPath newTreeInfo oldTreeInfo)
        addedDiffFileTree <- forM addedFiles (diffAddedTreeInfoFileInGivenPath newTreeInfo)
        removedDiffFileTree <- forM removedFiles (diffRemovedTreeInfoFileInGivenPath oldTreeInfo)

        return $ modifiedDiffFileTree ++ addedDiffFileTree ++ removedDiffFileTree

diffAddedTreeInfoFileInGivenPath :: TreeInfo -> FilePath -> IO DiffedFileTreeElement
diffAddedTreeInfoFileInGivenPath treeInfo path =
    do
        let foundAddedRepoTree = findRepoFileInTreeInfo treeInfo path
        case foundAddedRepoTree of
            (Just (RepoDir dirPath)) -> return $ DiffedDirectory AddedFile dirPath
            (Just (RepoFile filePath fileId)) ->
                getContentOfRepoFile fileId >>=
                    return . DiffedFile AddedFile filePath . flip diff [] . lines


diffRemovedTreeInfoFileInGivenPath :: TreeInfo -> FilePath -> IO DiffedFileTreeElement
diffRemovedTreeInfoFileInGivenPath treeInfo path =
    do
        let foundAddedRepoTree = findRepoFileInTreeInfo treeInfo path
        case foundAddedRepoTree of
            (Just (RepoDir dirPath)) -> return $ DiffedDirectory RemovedFile dirPath
            (Just (RepoFile filePath fileId)) ->
                getContentOfRepoFile fileId >>=
                    return . DiffedFile RemovedFile filePath . diff [] . lines

diffModifiedTreeInfoFileInGivenPath :: TreeInfo -> TreeInfo -> FilePath -> IO DiffedFileTreeElement
diffModifiedTreeInfoFileInGivenPath newTree oldTree path =
    do
        let (Just (RepoFile _ fileId1)) = findRepoFileInTreeInfo newTree path
            (Just (RepoFile _ fileId2)) = findRepoFileInTreeInfo oldTree path

        fileContent1 <- getContentOfRepoFile fileId1
        fileContent2 <- getContentOfRepoFile fileId2

        return $ DiffedFile ModifiedFile path $ diff (lines fileContent1) (lines fileContent2)










