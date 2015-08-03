module Main where

import System.Environment

import RepositoryActions

data Action =
    Init
    | Commit String
    | Log
    | GetVersion
    | Usage

parseArguments :: [String] -> Maybe Action
parseArguments ["init"] = Just Init
parseArguments ["commit",msg] = Just $ Commit msg
parseArguments ["log"] = Just Log
parseArguments ["--version"] = Just GetVersion
parseArguments ["--help"] = Just Usage
parseArguments [] = Just Usage
parseArguments badArgList = Nothing

doAction :: Maybe Action -> IO ()
doAction (Just Init) = initializeRepository
doAction (Just (Commit msg)) = commitRepository msg
doAction (Just Log) = logRepository
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction (Just Usage) = putStrLn "Usage: DarkHs.exe [--version|--help]"
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


























