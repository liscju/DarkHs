module Main where

import System.Environment

import RepositoryActions

data Action =
    Init
    | Commit String
    | Checkout Int
    | Log
    | GetVersion
    | Usage

parseArguments :: [String] -> Maybe Action
parseArguments ["init"] = Just Init
parseArguments ["commit", msg] = Just $ Commit msg
parseArguments ["checkout", commitId] = Just $ Checkout $ read commitId
parseArguments ["log"] = Just Log
parseArguments ["--version"] = Just GetVersion
parseArguments ["--help"] = Just Usage
parseArguments [] = Just Usage
parseArguments badArgList = Nothing

doAction :: Maybe Action -> IO ()
doAction (Just Init) = initializeRepository
doAction (Just (Commit msg)) = commitRepository msg
doAction (Just (Checkout commitId)) = checkoutRepository commitId
doAction (Just Log) = logRepository
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction (Just Usage) = putStrLn "Usage: DarkHs.exe [--version|--help]"
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


























