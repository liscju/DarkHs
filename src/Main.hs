module Main where

import System.Environment
import System.Directory
import Control.Exception
import System.FilePath
import Control.Monad
import System.IO

data Action =
    Init
    | GetVersion
    | Usage

parseArguments :: [String] -> Maybe Action
parseArguments ["init"] = Just Init
parseArguments ["--version"] = Just GetVersion
parseArguments ["--help"] = Just Usage
parseArguments [] = Just Usage
parseArguments badArgList = Nothing

doAction :: Maybe Action -> IO ()
doAction (Just Init) = initialize_repository
doAction (Just GetVersion) = putStrLn "DarkHs version 0.1.0.0"
doAction (Just Usage) = putStrLn "Usage: DarkHs.exe [--version|--help]"
doAction Nothing = putStrLn "Unrecognized command line arguments"

initial_directories :: [[FilePath]]
initial_directories = [
        [".darkhs"],
        [".darkhs","files"],
        [".darkhs","commits"],
        [".darkhs","trees"]
    ]

initialize_repository :: IO ()
initialize_repository =
    catch
        (do
            forM initial_directories $ createDirectory . joinPath
            withFile (joinPath [".darkhs","HEAD"]) AppendMode
                (flip hPutStr "0")
            putStrLn "Initialize repository in .darkhs")
        (\exc ->
            putStrLn $ "Initialize failed with exception :" ++
                            show (exc :: IOException))


main :: IO ()
main = getArgs >>= doAction . parseArguments


























