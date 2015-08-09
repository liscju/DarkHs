module Main where

import System.Environment
import Data.Char

import Repository
import RepositoryActions

parseArguments :: [String] -> Maybe RepoAction
parseArguments ["init"] = Just RepoInit
parseArguments ["commit", msg] = Just $ RepoCommit msg
parseArguments ["checkout", paramId]
    | isNumber (paramId !! 0) = Just $ RepoCheckoutByCommitId $ read paramId
    | otherwise               = Just $ RepoCheckoutByBranchId paramId
parseArguments ["branch", branchId] = Just $ RepoBranch branchId
parseArguments ["log"] = Just RepoLog
parseArguments ["diff"] = Just RepoDiffNotCommited
parseArguments ["diff", param1, param2]
    | isNumber (head param1) && isNumber (head param2)
        = Just $ RepoDiffCommits (read param1) (read param2)
    | isAlpha (head param1) && isAlpha (head param2)
        = Just $ RepoDiffBranches param1 param2
parseArguments ["rebase", branchId] = Just $ RepoRebase branchId
parseArguments ["status"] = Just RepoStatus
parseArguments ["--version"] = Just RepoGetVersion
parseArguments ["--help"] = Just RepoUsage
parseArguments [] = Just RepoUsage
parseArguments badArgList = Nothing

doAction :: Maybe RepoAction -> IO ()
doAction (Just action) = repositoryMakeAction action
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


























