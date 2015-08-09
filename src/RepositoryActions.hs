module RepositoryActions where

import Repository
import RepositoryFile
import Diff

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

resolvePendingOperation :: PendingOperation -> RepoAction -> IO ()
resolvePendingOperation (MergeConflictToResolve pathsToConflictsToFix) actionRequested =
    case actionRequested of
        RepoCommit msg -> do
            removePendingOperation
            repositoryDelegateAction actionRequested
        RepoStatus -> do
            putStrLn "Merge conflicts to resolve:"
            forM pathsToConflictsToFix $ \path -> putStrLn $ "\t" ++ path
            return ()
        _ -> putStrLn "You had to first resolve merge conflicts - see status for more information"

repositoryMakeAction :: RepoAction -> IO ()
repositoryMakeAction repoAction =
    do
        changeCurrentDirectoryToMainRepoDirectory
        maybePendingOperation <- getPendingOperation
        case maybePendingOperation of
            Just pendingOperation ->
                resolvePendingOperation pendingOperation repoAction
            Nothing ->
                repositoryDelegateAction repoAction

repositoryDelegateAction :: RepoAction -> IO ()
repositoryDelegateAction (RepoInit) = initializeRepository
repositoryDelegateAction (RepoCommit msg) = commitRepository msg
repositoryDelegateAction (RepoCheckoutByCommitId commitId) = checkoutRepositoryByCommitId commitId
repositoryDelegateAction (RepoCheckoutByBranchId branchId) = checkoutRepositoryByBranchId branchId
repositoryDelegateAction (RepoBranch branchId) = branchRepository branchId
repositoryDelegateAction (RepoLog) = logRepository
repositoryDelegateAction (RepoDiffNotCommited) = diffNotCommitedRepository
repositoryDelegateAction (RepoDiffCommits newCommit oldCommit) = diffCommits newCommit oldCommit
repositoryDelegateAction (RepoDiffBranches newBranch oldBranch) = diffBranches newBranch oldBranch
repositoryDelegateAction (RepoRebase branchId) = rebaseBranchRepository branchId
repositoryDelegateAction (RepoStatus) = statusRepository
repositoryDelegateAction (RepoGetVersion) = putStrLn "DarkHs version 0.1.0.0"
repositoryDelegateAction (RepoUsage) = putStrLn "Usage: DarkHs.exe [--version|--help]"

repositoryActionHandler :: IO a -> (IOError -> IO a) -> IO ()
repositoryActionHandler action excHandler =
    do
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
            nextTreeId <- generateNextTreeId
            repoTreeFileList <- copyWorkingDirectoryToRepoFiles
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
            commits <- getCurrentCommitId >>= getCommitHistory
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
            saveHeadToFile commitId)
        (\exc ->
            putStrLn $ "Log failed with exception :" ++
                show (exc :: IOException) )

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
        (do
            getCurrentCommitId >>= createNewBranch branchId
            putStrLn $ "Branch " ++ branchId ++ " succesfully created")
        (\exc ->
            putStrLn $ "Checkout failed with exception :" ++
                                        show (exc :: IOException) )

printCurrentBranch :: IO ()
printCurrentBranch =
    do
        currentBranchPointer <- getCurrentBranchPointer
        currentBranchMsg <- case currentBranchPointer of
                                BranchPointer branchId ->
                                    return $ "On branch " ++ branchId
                                DetachedPointer ->
                                    getCurrentCommitId >>= return . (++) "On detached commit #" . show
        putStrLn currentBranchMsg

printWorkingDirectoryStatus :: IO ()
printWorkingDirectoryStatus =
    do
        workingDirectoryTree <- copyWorkingDirectoryToRepoFiles
        headDirectoryTree <- getCurrentCommitId >>= getTreeInfoForCommitId

        let filesTreeComparison = compareTreeInfos workingDirectoryTree headDirectoryTree
        (modifiedFiles, addedFiles, removedFiles, unmodifiedFiles) <- filesTreeComparison

        case (modifiedFiles, addedFiles, removedFiles, unmodifiedFiles) of
            ([],[],[],[]) ->
                putStrLn "nothing to commit, working directory clean"
            otherwise ->
                do
                    forM modifiedFiles $ putStrLn .   (++)  "\t\tmodified :\t"
                    forM addedFiles $ putStrLn    .   (++)  "\t\tadded    :\t"
                    forM removedFiles $ putStrLn  .   (++)  "\t\tremoved  :\t"
                    forM unmodifiedFiles $ putStrLn . (++)  "\t\tunmodified: \t"
                    return ()

        putStrLn ""

statusRepository :: IO ()
statusRepository =
    repositoryActionHandler
        (do
            printCurrentBranch
            printWorkingDirectoryStatus)
        (\exc ->
            putStrLn $ "Status failed with exception :" ++
                                        show (exc :: IOException) )

showDifferenceBetweenTreeInfos :: TreeInfo -> TreeInfo -> IO ()
showDifferenceBetweenTreeInfos newTreeInfo oldTreeInfo =
    do
        diffedFileTree <- diffTreeInfos newTreeInfo oldTreeInfo
        putStrLn $ prettyPrintDiffedFileTree diffedFileTree

diffNotCommitedRepository :: IO ()
diffNotCommitedRepository =
    repositoryActionHandler
        (do
            workingDirectoryTree <- copyWorkingDirectoryToRepoFiles
            headDirectoryTree <- getCurrentCommitId >>= getTreeInfoForCommitId
            showDifferenceBetweenTreeInfos workingDirectoryTree headDirectoryTree)
        (\exc ->
            putStrLn $ "Diff failed with exception :" ++
                                        show (exc :: IOException) )

diffCommits :: CommitId -> CommitId -> IO ()
diffCommits newCommit oldCommit =
    repositoryActionHandler
        (do
            newDirectoryTree <- getTreeInfoForCommitId newCommit
            oldDirectoryTree <- getTreeInfoForCommitId oldCommit
            showDifferenceBetweenTreeInfos newDirectoryTree oldDirectoryTree)
        (\exc ->
            putStrLn $ "Diff failed with exception :" ++
                                        show (exc :: IOException) )

diffBranches :: BranchId -> BranchId -> IO ()
diffBranches newBranch oldBranch =
    repositoryActionHandler
        (do
            newDirectoryTree <- getBranchCommit newBranch >>= getTreeInfoForCommitId
            oldDirectoryTree <- getBranchCommit oldBranch >>= getTreeInfoForCommitId
            showDifferenceBetweenTreeInfos newDirectoryTree oldDirectoryTree)
        (\exc ->
            putStrLn $ "Diff failed with exception :" ++
                                        show (exc :: IOException) )

rebaseBranchRepository :: BranchId -> IO ()
rebaseBranchRepository branchId =
    repositoryActionHandler
        (do
            fastForwardResult <- tryFastForwardMergeToBranch branchId
            case fastForwardResult of
                FastForwardNothingToMerge ->
                    putStrLn "Merge: Fast forward nothing to merge"
                FastForwardMerged ->
                    putStrLn "Merge: Fast forward merged"
                FastForwardNotApplicable -> do
                    putStrLn "Merge: Fast forward not applicable, calculating typical rebase"
                    tryRebaseMergeToBranch branchId
            return ())
        (\exc ->
            putStrLn $ "Diff failed with exception :" ++
                                        show (exc :: IOException) )





















