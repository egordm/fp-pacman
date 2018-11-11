module Game.File.Fileable (
    Fileable(..), 
    FileInstruction(..), 
    ReadFileResult(..),
    executeFileInstructions,
    emptyReadFileResult,
    fromReadFileResult
) where

import qualified Data.Map as Map

class Fileable a where
    doFile :: a -> [FileInstruction]

data FileInstruction = FileWriteInst {
    fileName :: String,
    fileContent :: String
} | FileReadInst {
    fileName :: String,
    destName :: String
}

type ReadFileResult = Map.Map String String

emptyReadFileResult :: ReadFileResult
emptyReadFileResult = Map.empty

fromReadFileResult :: String -> ReadFileResult -> Maybe String
fromReadFileResult name rfr = Map.lookup name rfr

executeFileInstructions :: [FileInstruction] -> IO ReadFileResult
executeFileInstructions inst = do
    raw <- executeFileInstructions_ inst
    return $ Map.fromList raw

executeFileInstructions_ :: [FileInstruction] -> IO [(String, String)]
executeFileInstructions_ [] = return []
executeFileInstructions_ (x:xs) = case x of
    FileWriteInst{fileName, fileContent} -> do 
        writeFile fileName fileContent
        executeFileInstructions_ xs
    FileReadInst{fileName, destName} -> do 
        str <- readFile fileName
        rts <- executeFileInstructions_ xs
        return $ (destName, str) : rts

