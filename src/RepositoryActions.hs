module RepositoryActions where

import Repository
import RepositoryFile

import System.Directory
import Control.Exception
import System.FilePath
import Control.Monad
import System.IO

initialRepositoryDirectories :: [FilePath]
initialRepositoryDirectories = [
        topdir,
        filesDir,
        commitsDir,
        treesDir,
        branchesDir
    ]

initialHeadCommitId :: CommitId
initialHeadCommitId = 0

initialCurrentBranchPointer :: CurrentBranchPointer
initialCurrentBranchPointer = BranchPointer "master"

initialRepositoryFiles :: [(FilePath,String)]
initialRepositoryFiles = [
        (headFile, show initialHeadCommitId),
        (currentBranchPointerFile, show initialCurrentBranchPointer),
        (masterBranchFile, show initialHeadCommitId)
    ]

repositoryActionHandler :: IO a -> (IOError -> IO a) -> IO ()
repositoryActionHandler action excHandler =
    do
        changeCurrentDirectoryToMainRepoDirectory
        catch
            action
            excHandler
        return ()


initializeRepository :: IO ()
initializeRepository =
    catch
        (do
            forM initialRepositoryDirectories createDirectory
            forM initialRepositoryFiles $ \(path,value) -> do
                writeFile path value
            putStrLn "Initialize repository in .darkhs")
        (\exc ->
            putStrLn $ "Initialize failed with exception :" ++
                            show (exc :: IOException))

commitRepository :: String -> IO ()
commitRepository msg =
    repositoryActionHandler
        (do
            currentCommitId <- getCurrentCommitId
            nextCommitId <- generateNextCommitId
            nextFileId <- generateNextFileId
            nextTreeId <- generateNextTreeId
            repoTreeFileList <- copyWorkingDirectoryToRepoFiles nextFileId
            saveTreeToFile nextTreeId repoTreeFileList
            let commit = CommitInfo msg currentCommitId nextTreeId
            saveCommitToFile nextCommitId commit
            saveHeadToFile nextCommitId
            updateCurrentBranchCommit nextCommitId
            putStrLn $ "[" ++ msg ++ "]\n\t commited succesfully with ID#" ++ show nextCommitId
            return () )
        (\exc ->
            putStrLn $ "Commiting failed with exception :" ++
                                        show (exc :: IOException) )


logRepository :: IO ()
logRepository =
    repositoryActionHandler
        (do
            commits <- getCurrentCommitId >>= getLogInformation
            forM commits $ \commit -> do
                putStrLn $ logCommitShow commit
            return () )
        (\exc ->
            putStrLn $ "Log failed with exception :" ++
                                        show (exc :: IOException) )

checkoutRepository :: CommitId -> IO ()
checkoutRepository commitId =
    do
        commitInfo <- getCommitInformation commitId
        let commitTreeId = getCommitTreeId commitInfo
        commitTreeInfo <- getTreeInfo commitTreeId
        clearCurrentWorkingDirectory
        copyRepoFilesToWorkingDirectory commitTreeInfo
        saveHeadToFile commitId

checkoutRepositoryByCommitId :: CommitId -> IO ()
checkoutRepositoryByCommitId commitId =
    repositoryActionHandler
        (do
            checkoutRepository commitId
            detachCurrentBranchPointer
            putStrLn $ "Checkout succesfully to #" ++ (show commitId)
            return ())
        (\exc ->
            putStrLn $ "Checkout failed with exception :" ++
                                        show (exc :: IOException) )

checkoutRepositoryByBranchId :: BranchId -> IO ()
checkoutRepositoryByBranchId branchId =
    repositoryActionHandler
        (do
            commitId <- getBranchCommit branchId
            checkoutRepository commitId
            attachCurrentBranchPointer branchId
            putStrLn $ "Checkout succesfully to branch: " ++ branchId
            return ())
        (\exc ->
            putStrLn $ "Checkout failed with exception :" ++
                                        show (exc :: IOException) )

branchRepository :: BranchId -> IO ()
branchRepository branchId =
    repositoryActionHandler
        (getCurrentCommitId >>= createNewBranch branchId)
        (\exc ->
            putStrLn $ "Checkout failed with exception :" ++
                                        show (exc :: IOException) )






















