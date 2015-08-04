module Repository where

import Data.List

type CommitId = Int
type CommitMsg = String

type TreeId = Int
type FileId = Int

data RepoTreeFile =
    RepoDir FilePath
    | RepoFile FilePath FileId
    deriving (Read, Show)

type TreeInfo = [RepoTreeFile]

-- CommitInfo CommitMsg PreviousCommitId TreeId
data CommitInfo = CommitInfo CommitMsg CommitId TreeId
    deriving (Read, Show)

type Commit = (CommitId, CommitInfo)

type Tree = (TreeId, TreeInfo)

type BranchId = String

data Branch = Branch BranchId CommitId

data CurrentBranchPointer =
    BranchPointer BranchId
    | DetachedPointer
    deriving (Read, Show)

-- FiletreesComparison (Modified,Added,Removed)
type TreeInfoComparison =
    ([FilePath],[FilePath],[FilePath])


getParentCommit :: CommitInfo -> CommitId
getParentCommit (CommitInfo _ parent _) = parent

getCommitTreeId :: CommitInfo -> TreeId
getCommitTreeId (CommitInfo _ _ treeId) = treeId

logCommitShow :: Commit -> String
logCommitShow commit =
    let (commitId, (CommitInfo msg _ _)) = commit in
        "commit " ++ show commitId ++ "\n\t" ++ msg ++ "\n"

getRepoTreeFilePath :: RepoTreeFile -> FilePath
getRepoTreeFilePath (RepoDir filePath) = filePath
getRepoTreeFilePath (RepoFile filePath _) = filePath

findRepoFileInTreeInfo :: TreeInfo -> FilePath -> Maybe RepoTreeFile
findRepoFileInTreeInfo treeInfo path = find (((==) path) . getRepoTreeFilePath) treeInfo

