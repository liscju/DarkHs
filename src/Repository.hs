module Repository where

type CommitId = Int
type CommitMsg = String

type TreeId = Int
type FileId = Int

data RepoTreeFile =
    RepoDir FilePath
    | RepoFile FilePath FileId
    deriving (Show)

-- CommitInfo CommitMsg PreviousCommitId TreeId
data CommitInfo = CommitInfo CommitMsg CommitId TreeId
    deriving (Read, Show)

type Commit = (CommitId, CommitInfo)

getParentCommit :: CommitInfo -> CommitId
getParentCommit (CommitInfo _ parent _) = parent

logCommitShow :: Commit -> String
logCommitShow commit =
    let (commitId, (CommitInfo msg _ _)) = commit in
        "commit " ++ show commitId ++ "\n\t" ++ msg ++ "\n"

