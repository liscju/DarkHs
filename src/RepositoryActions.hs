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
        treesDir
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
    repositoryActionHandler
        (do
            forM initialRepositoryDirectories createDirectory
            saveHeadToFile initialHeadCommitId
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
    repositoryActionHandler
        (do
            commitInfo <- getCommitInformation commitId
            let commitTreeId = getCommitTreeId commitInfo
            commitTreeInfo <- getTreeInfo commitTreeId
            clearCurrentWorkingDirectory
            copyRepoFilesToWorkingDirectory commitTreeInfo
            saveHeadToFile commitId
            putStrLn $ "Checkout succesfully to #" ++ (show commitId)
            return ())
        (\exc ->
            putStrLn $ "Checkout failed with exception :" ++
                                        show (exc :: IOException) )
























