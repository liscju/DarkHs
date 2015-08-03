module Repository where

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

getParentCommit :: CommitInfo -> CommitId
getParentCommit (CommitInfo _ parent _) = parent

getCommitTreeId :: CommitInfo -> TreeId
getCommitTreeId (CommitInfo _ _ treeId) = treeId

logCommitShow :: Commit -> String
logCommitShow commit =
    let (commitId, (CommitInfo msg _ _)) = commit in
        "commit " ++ show commitId ++ "\n\t" ++ msg ++ "\n"

