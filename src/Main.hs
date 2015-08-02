module Main where

import System.Environment

data Action =
    GetVersion

parseArguments :: [String] -> Maybe Action
parseArguments ["--version"] = Just GetVersion
parseArguments badArgList = Nothing

doAction :: Maybe Action -> IO ()
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction Nothing = putStrLn "Unrecognized command line arguments"

main :: IO ()
main = getArgs >>= doAction . parseArguments


