module Repository where

import Data.List
import Data.Maybe

type CommitId = Int
type CommitMsg = String

type TreeId = Int
type FileId = Int

data RepoTreeFile =
    RepoFile FilePath FileId
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

-- FiletreesComparison (Modified,Added,Removed,Unmodified)
type TreeInfoComparison =
    ([FilePath],[FilePath],[FilePath],[FilePath])

data FastForwardMergeResult =
    FastForwardMerged
    | FastForwardNothingToMerge
    | FastForwardNotApplicable

data ThreeWayCommitRebaseMergeResult =
    ThreeWayCommitRebaseMerged
    | ThreeWayCommitRebaseConflictsToResolved

getParentCommit :: CommitInfo -> CommitId
getParentCommit (CommitInfo _ parent _) = parent

getCommitTreeId :: CommitInfo -> TreeId
getCommitTreeId (CommitInfo _ _ treeId) = treeId

logCommitShow :: Commit -> String
logCommitShow commit =
    let (commitId, (CommitInfo msg _ _)) = commit in
        "commit " ++ show commitId ++ "\n\t" ++ msg ++ "\n"

getRepoTreeFilePath :: RepoTreeFile -> FilePath
getRepoTreeFilePath (RepoFile filePath _) = filePath

findRepoFileInTreeInfo :: TreeInfo -> FilePath -> Maybe RepoTreeFile
findRepoFileInTreeInfo treeInfo path = find (((==) path) . getRepoTreeFilePath) treeInfo

findYoungestCommonCommitAncestor :: [Commit] -> [Commit] -> CommitId
findYoungestCommonCommitAncestor commitHistory1 commitHistory2 =
    case foundAncestor of
        Nothing -> 0
        Just (commitId, _) -> commitId
    where
        foundAncestor = find (isJust . flip find commitHistory2 . hasSameCommitId) commitHistory1
        hasSameCommitId (commitId1, _) (commitId2, _) =
            commitId1 == commitId2


















