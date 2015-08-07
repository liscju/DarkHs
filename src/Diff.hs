module Diff where

import Data.Maybe
import Data.List

type Line = String

data LineOperation = Unchanged | Added | Removed
    deriving (Show)

data DiffedLine = DiffedLine LineOperation Line
    deriving (Show)

type FileContent = [Line]

type DiffedFileContent = [DiffedLine]

data FileChanged =
    ModifiedFile | AddedFile | RemovedFile | UnchangedFile
    deriving(Eq)

data DiffedFileTreeElement =
    DiffedDirectory FileChanged FilePath
    | DiffedFile FileChanged FilePath DiffedFileContent

type DiffedFileTree = [DiffedFileTreeElement]

getPathFromDiffedFileTreeElement :: DiffedFileTreeElement -> FilePath
getPathFromDiffedFileTreeElement (DiffedDirectory _ path) = path
getPathFromDiffedFileTreeElement (DiffedFile _ path _) = path

getFileChangedFromDiffedFileTreeElement :: DiffedFileTreeElement -> FileChanged
getFileChangedFromDiffedFileTreeElement (DiffedDirectory fileChanged _) = fileChanged
getFileChangedFromDiffedFileTreeElement (DiffedFile fileChanged _ _) = fileChanged

diff :: FileContent -> FileContent -> DiffedFileContent
diff [] [] = []
diff [] (currLine:restLines) = (DiffedLine Removed currLine) : diff [] restLines
diff (currLine:restLines) [] = (DiffedLine Added   currLine) : diff restLines []
diff (currNewLine:restNewLines) (currOldLine:restOldLines) =
    if currNewLine == currOldLine
        then (DiffedLine Unchanged currNewLine) : diff restNewLines restOldLines
        else
            if isJust $ find ((==) currOldLine) restNewLines
                then
                    (DiffedLine Added currNewLine) : diff restNewLines (currOldLine:restOldLines)
                else
                    if isJust $ find((==) currNewLine) restOldLines
                        then (DiffedLine Removed currOldLine) : diff (currNewLine:restNewLines) restOldLines
                        else (DiffedLine Removed currOldLine) : (DiffedLine Added currNewLine) : diff restNewLines restOldLines

prettyPrintDiffedFileContent :: DiffedFileContent -> String
prettyPrintDiffedFileContent =
    unlines . map diffedLineToString
    where
        diffedLineToString :: DiffedLine -> String
        diffedLineToString (DiffedLine Unchanged line) = "   " ++ line
        diffedLineToString (DiffedLine Added     line) = "++ " ++ line
        diffedLineToString (DiffedLine Removed   line) = "-- " ++ line

prettyPrintDiffedFileTree :: DiffedFileTree -> String
prettyPrintDiffedFileTree diffedFileTree =
    unlines $ map prettyPrintDiffedFileTreeElement diffedFileTree

prettyPrintDiffedFileTreeElement :: DiffedFileTreeElement -> String
prettyPrintDiffedFileTreeElement diffedDir@(DiffedDirectory _ _) =
    prettyPrintDiffedDirectory diffedDir
prettyPrintDiffedFileTreeElement diffedFile@(DiffedFile _ _ _) =
    prettyPrintDiffedFile diffedFile

prettyPrintFileChanged :: FileChanged -> String
prettyPrintFileChanged ModifiedFile = "modified"
prettyPrintFileChanged AddedFile = "added"
prettyPrintFileChanged RemovedFile = "removed"
prettyPrintFileChanged UnchangedFile = "unchanged"

prettyPrintDiffedDirectory :: DiffedFileTreeElement -> String
prettyPrintDiffedDirectory (DiffedDirectory fileChanged filePath) =
    "///////////////////////\n" ++
    "Directory " ++ filePath ++ " was " ++ prettyPrintFileChanged fileChanged ++ "\n" ++
    "///////////////////////\n"

prettyPrintDiffedFile :: DiffedFileTreeElement -> String
prettyPrintDiffedFile (DiffedFile fileChanged filePath diffedFileContent) =
    "///////////////////////\n" ++
    "File " ++ filePath ++ " was " ++ prettyPrintFileChanged fileChanged ++ "\n" ++
    "-----------------------\n" ++
    prettyPrintDiffedFileContent diffedFileContent ++
    "///////////////////////\n"
























