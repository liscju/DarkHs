module Main where

import System.Environment

data Action =
    GetVersion

parseArguments :: [String] -> Maybe Action
parseArguments ["--version"] = Just GetVersion
parseArguments badArgList = Nothing

main :: IO ()
main =
    do
        args <- getArgs
        let actionToDo = parseArguments args
        case actionToDo of
            Just GetVersion -> putStrLn "DarkHs version 0.1.0.0"
            Nothing -> putStrLn "Unrecognized command line arguments"


