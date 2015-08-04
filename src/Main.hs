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
    | Log
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
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction (Just Usage) = putStrLn "Usage: DarkHs.exe [--version|--help]"
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


























