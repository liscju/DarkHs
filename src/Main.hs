module Main where

import System.Environment
import Data.Char

import RepositoryActions

data Action =
    Init
    | Commit String
    | CheckoutByCommitId Int
    | CheckoutByBranchId String
    | Branch String
    | DiffNotCommited
    | DiffCommits Int Int
    | DiffBranches String String
    | Log
    | Status
    | GetVersion
    | Usage

parseArguments :: [String] -> Maybe Action
parseArguments ["init"] = Just Init
parseArguments ["commit", msg] = Just $ Commit msg
parseArguments ["checkout", paramId]
    | isNumber (paramId !! 0) = Just $ CheckoutByCommitId $ read paramId
    | otherwise               = Just $ CheckoutByBranchId paramId
parseArguments ["branch", branchId] = Just $ Branch branchId
parseArguments ["log"] = Just Log
parseArguments ["diff"] = Just DiffNotCommited
parseArguments ["diff", param1, param2]
    | isNumber (head param1) && isNumber (head param2)
        = Just $ DiffCommits (read param1) (read param2)
    | isAlpha (head param1) && isAlpha (head param2)
        = Just $ DiffBranches param1 param2
parseArguments ["status"] = Just Status
parseArguments ["--version"] = Just GetVersion
parseArguments ["--help"] = Just Usage
parseArguments [] = Just Usage
parseArguments badArgList = Nothing

doAction :: Maybe Action -> IO ()
doAction (Just Init) = initializeRepository
doAction (Just (Commit msg)) = commitRepository msg
doAction (Just (CheckoutByCommitId commitId)) = checkoutRepositoryByCommitId commitId
doAction (Just (CheckoutByBranchId branchId)) = checkoutRepositoryByBranchId branchId
doAction (Just (Branch branchId)) = branchRepository branchId
doAction (Just Log) = logRepository
doAction (Just DiffNotCommited) = diffNotCommitedRepository
doAction (Just (DiffCommits newCommit oldCommit)) = diffCommits newCommit oldCommit
doAction (Just (DiffBranches newBranch oldBranch)) = diffBranches newBranch oldBranch
doAction (Just Status) = statusRepository
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction (Just Usage) = putStrLn "Usage: DarkHs.exe [--version|--help]"
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


























