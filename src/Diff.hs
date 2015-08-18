module Diff where

import Data.Maybe
import Data.List
import Data.Algorithm.Diff
import Data.Algorithm.Diff3
import Debug.Trace

type Line = String

data LineOperation = UnchangedLine | AddedLine | RemovedLine
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

getPathOfRepoTreeFileContent :: RepoTreeFileContent -> FilePath
getPathOfRepoTreeFileContent (RepoFileContent filePath _) = filePath

getDiffedFileContentFromDiffedFileTreeElement :: DiffedFileTreeElement -> DiffedFileContent
getDiffedFileContentFromDiffedFileTreeElement (DiffedFile _ _ diffedFileContent) = diffedFileContent

convertDiffedFileTreeElementToRepoTreeFileContent :: DiffedFileTreeElement -> Maybe RepoTreeFileContent
convertDiffedFileTreeElementToRepoTreeFileContent (DiffedFile fileChanged filePath diffedFileContent)
    | fileChanged == RemovedFile = Nothing
    | otherwise = Just $ RepoFileContent filePath (getActualContent diffedFileContent)

getActualContent :: DiffedFileContent -> String
getActualContent = unlines .
    foldl (\currFileContent diffedNextLine -> currFileContent ++ actualLineContent diffedNextLine) []
    where actualLineContent (DiffedLine lineOperation line)
            | lineOperation == RemovedLine = []
            | otherwise = [line]

diff :: FileContent -> FileContent -> DiffedFileContent
diff newFile oldFile =
    map convertToDiffedLine $ getDiff newFile oldFile
    where
        convertToDiffedLine :: Diff String -> DiffedLine
        convertToDiffedLine (First str)  = DiffedLine AddedLine   str
        convertToDiffedLine (Second str) = DiffedLine RemovedLine str
        convertToDiffedLine (Both str _) = DiffedLine UnchangedLine str

prettyPrintDiffedFileContent :: DiffedFileContent -> String
prettyPrintDiffedFileContent =
    unlines . map diffedLineToString
    where
        diffedLineToString :: DiffedLine -> String
        diffedLineToString (DiffedLine UnchangedLine line) = "   " ++ line
        diffedLineToString (DiffedLine AddedLine     line) = "++ " ++ line
        diffedLineToString (DiffedLine RemovedLine   line) = "-- " ++ line

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
                      [RepoTreeFileContent] ->
                      [Either RepoTreeFileContent RepoTreeFileContent]
mergeDiffFileTrees newDiffFileTree oldDiffFileTree baseRepoTreeFileContents =
    map rightJust $ filter notRightNothing $
        map mergeDiffFileTreeElementWithSamePath allTriplesOfDiffedFileTreeElementWithSamePath
    where
        allPaths = nub $ map getPathFromDiffedFileTreeElement $ newDiffFileTree ++ oldDiffFileTree
        diffFileTreeElements path = (find ((==) path . getPathFromDiffedFileTreeElement) newDiffFileTree,
                                     find ((==) path . getPathFromDiffedFileTreeElement) oldDiffFileTree,
                                     find ((==) path . getPathOfRepoTreeFileContent) baseRepoTreeFileContents)
        allTriplesOfDiffedFileTreeElementWithSamePath = map diffFileTreeElements allPaths

        notRightNothing (Right Nothing) = False
        notRightNothing _               = True

        rightJust (Right x) = Right $ fromJust x
        rightJust (Left x)  = Left x

-- diffedfiletreelements must have been diffed by common ancestor
mergeDiffFileTreeElementWithSamePath ::
                            (Maybe DiffedFileTreeElement,
                             Maybe DiffedFileTreeElement,
                             Maybe RepoTreeFileContent) ->
                            Either RepoTreeFileContent (Maybe RepoTreeFileContent)
mergeDiffFileTreeElementWithSamePath
    (newMaybeDiffTreeElement,oldMaybeDiffTreeElement,baseRepoTreeFileContent) =
        case (newMaybeDiffTreeElement >>= Just . getFileChangedFromDiffedFileTreeElement ,
              oldMaybeDiffTreeElement >>= Just . getFileChangedFromDiffedFileTreeElement) of
                (Just AddedFile, Nothing) -> Right $
                    newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Nothing, Just AddedFile) -> Right $
                    oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Just AddedFile, Just _)  -> tryToResolveConflictResult
                (Just _, Just AddedFile)  -> tryToResolveConflictResult
                (Just RemovedFile, Just UnchangedFile) -> Right $
                    newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Just UnchangedFile, Just RemovedFile) -> Right $
                    oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Just RemovedFile, Just RemovedFile) -> Right $ Nothing
                (Just RemovedFile, _) -> tryToResolveConflictResult
                (_, Just RemovedFile) -> tryToResolveConflictResult
                (Just ModifiedFile, Just UnchangedFile) -> Right $
                    newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Just UnchangedFile, Just ModifiedFile) -> Right $
                    oldMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
                (Just ModifiedFile, x) -> tryToResolveConflictResult
                (x, Just ModifiedFile) -> tryToResolveConflictResult
                (Just UnchangedFile,Just UnchangedFile) -> Right $
                    newMaybeDiffTreeElement >>= convertDiffedFileTreeElementToRepoTreeFileContent
        where
            baseRepoFileContent Nothing = ""
            baseRepoFileContent (Just (RepoFileContent _ content)) = content
            tryToResolveConflictResult =
                tryResolvingFileContentMergeConflict
                    (getActualContent $ getDiffedFileContentFromDiffedFileTreeElement $ fromJust newMaybeDiffTreeElement)
                    (getActualContent $ getDiffedFileContentFromDiffedFileTreeElement $ fromJust oldMaybeDiffTreeElement)
                    (baseRepoFileContent baseRepoTreeFileContent)
                    (getPathFromDiffedFileTreeElement $ fromJust newMaybeDiffTreeElement)

-- try resolve conflict on file content level
tryResolvingFileContentMergeConflict :: String -> String -> String -> FilePath
    -> Either RepoTreeFileContent (Maybe RepoTreeFileContent)
tryResolvingFileContentMergeConflict
    mainContent mergeContent baseContent path =
    case merge (diff3 mainContentLines baseContentLines mergeContentLines) of
        Left hunks -> Left $ RepoFileContent path $ unlines $ map hunkToString hunks
        Right xs -> Right $ Just $ RepoFileContent path  $ unlines xs
    where
        mainContentLines = lines mainContent
        mergeContentLines = lines mergeContent
        baseContentLines    = lines baseContent
        hunkToString (LeftChange xs) = unlines xs
        hunkToString (RightChange xs) = unlines xs
        hunkToString (Unchanged xs) = unlines xs
        hunkToString (Conflict newLines baseLines oldLines) =
            "New Version:\n" ++
                "=======================================\n" ++
                (unlines newLines) ++
                "\n=======================================\n" ++
                (unlines oldLines) ++
                "\n=======================================\n" ++
                "Old Version"




















