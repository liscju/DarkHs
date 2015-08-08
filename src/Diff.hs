module Diff where

import Data.Maybe
import Data.List

type Line = String

data LineOperation = Unchanged | Added | Removed
    deriving (Eq,Show)

data DiffedLine = DiffedLine LineOperation Line
    deriving (Show)

type FileContent = [Line]

type DiffedFileContent = [DiffedLine]

data FileChanged =
    ModifiedFile | AddedFile | RemovedFile | UnchangedFile
    deriving(Eq)

data DiffedFileTreeElement =
    DiffedFile FileChanged FilePath DiffedFileContent

type DiffedFileTree = [DiffedFileTreeElement]

data RepoTreeFileContent =
    RepoFileContent FilePath String
    deriving (Show)

getPathFromDiffedFileTreeElement :: DiffedFileTreeElement -> FilePath
getPathFromDiffedFileTreeElement (DiffedFile _ path _) = path

getFileChangedFromDiffedFileTreeElement :: DiffedFileTreeElement -> FileChanged
getFileChangedFromDiffedFileTreeElement (DiffedFile fileChanged _ _) = fileChanged

convertDiffedFileTreeElementToRepoTreeFileContent :: DiffedFileTreeElement -> Maybe RepoTreeFileContent
convertDiffedFileTreeElementToRepoTreeFileContent (DiffedFile fileChanged filePath diffedFileContent)
    | fileChanged == RemovedFile = Nothing
    | otherwise = Just $ RepoFileContent filePath (getActualContent diffedFileContent)

getActualContent :: DiffedFileContent -> String
getActualContent = unlines .
    foldl (\currFileContent diffedNextLine -> currFileContent ++ actualLineContent diffedNextLine) []
    where actualLineContent (DiffedLine lineOperation line)
            | lineOperation == Removed = []
            | otherwise = [line]

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
prettyPrintDiffedFileTreeElement diffedFile@(DiffedFile _ _ _) =
    prettyPrintDiffedFile diffedFile

prettyPrintFileChanged :: FileChanged -> String
prettyPrintFileChanged ModifiedFile = "modified"
prettyPrintFileChanged AddedFile = "added"
prettyPrintFileChanged RemovedFile = "removed"
prettyPrintFileChanged UnchangedFile = "unchanged"

prettyPrintDiffedFile :: DiffedFileTreeElement -> String
prettyPrintDiffedFile (DiffedFile fileChanged filePath diffedFileContent) =
    "///////////////////////\n" ++
    "File " ++ filePath ++ " was " ++ prettyPrintFileChanged fileChanged ++ "\n" ++
    "-----------------------\n" ++
    prettyPrintDiffedFileContent diffedFileContent ++
    "///////////////////////\n"


-- newDiffFileTree,oldDiffFileTree mierzone od tego samego ancestora
-- either left->merge conflict right->ok file
mergeDiffFileTrees :: DiffedFileTree ->
                      DiffedFileTree ->
                      [Either RepoTreeFileContent (Maybe RepoTreeFileContent)]
mergeDiffFileTrees newDiffFileTree oldDiffFileTree =
    map mergeDiffFileTreeElementWithSamePath allPairsOfDiffedFileTreeElementWithSamePath
    where
        allPaths = nub $ map getPathFromDiffedFileTreeElement $ newDiffFileTree ++ oldDiffFileTree
        diffFileTreeElements path = (find ((==) path . getPathFromDiffedFileTreeElement) newDiffFileTree,
                                     find ((==) path . getPathFromDiffedFileTreeElement) oldDiffFileTree)
        allPairsOfDiffedFileTreeElementWithSamePath = map diffFileTreeElements allPaths

-- diffedfiletreelements must have been diffed by common ancestor
mergeDiffFileTreeElementWithSamePath :: (Maybe DiffedFileTreeElement, Maybe DiffedFileTreeElement) ->
                            Either RepoTreeFileContent (Maybe RepoTreeFileContent)
mergeDiffFileTreeElementWithSamePath (newMaybeDiffTreeElement,oldMaybeDiffTreeElement) =
    case (newMaybeDiffTreeElement >>= Just . getFileChangedFromDiffedFileTreeElement ,
          oldMaybeDiffTreeElement >>= Just . getFileChangedFromDiffedFileTreeElement) of
            (Just AddedFile, Nothing) -> Right $
                newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Nothing, Just AddedFile) -> Right $
                oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Just AddedFile, Just _)  ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)
            (Just _, Just AddedFile)  ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)

            (Just RemovedFile, Just UnchangedFile) -> Right $
                newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Just UnchangedFile, Just RemovedFile) -> Right $
                oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Just RemovedFile, Just RemovedFile) -> Right $ Nothing
            (Just RemovedFile, _) ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)
            (_, Just RemovedFile) ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)

            (Just ModifiedFile, Just UnchangedFile) -> Right $
                newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Just UnchangedFile, Just ModifiedFile) -> Right $
                oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
            (Just ModifiedFile, x) ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)
            (x, Just ModifiedFile) ->
                tryToResolveConflictResult (fromJust newMaybeDiffTreeElement) (fromJust oldMaybeDiffTreeElement)

            (Just UnchangedFile,Just UnchangedFile) -> Right $
                newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
    where
                tryToResolveConflictResult =
                    tryResolvingMergeConflict

-- try resolve conflict on file content level
tryResolvingMergeConflict :: DiffedFileTreeElement ->
                             DiffedFileTreeElement ->
                             Either RepoTreeFileContent (Maybe RepoTreeFileContent)
tryResolvingMergeConflict (DiffedFile newFileChanged newFilePath newDiffedFileContent)
                          (DiffedFile oldFileChanged oldFilePath oldDiffedFileContent)
    | newFilePath /= oldFilePath = error "Resolving conflict on file level only - args must have same path"
    | otherwise = Left $ RepoFileContent newFilePath $
        "New Version:\n" ++
        "=======================================\n" ++
        getActualContent newDiffedFileContent ++
        "\n=======================================\n" ++
        getActualContent oldDiffedFileContent ++
        "\n=======================================\n" ++
        "Old Version"




















