module FileProcessor
    ( findTextFiles
    , processFile
    , processAndPrintFile
    ) where

import System.Directory
import Control.Monad
import Data.List (isSuffixOf)
import Types
import Parser
import Sorter

findTextFiles :: IO [FilePath]
findTextFiles = do
    files <- listDirectory "."
    filterM (\f -> do
        exists <- doesFileExist f
        return $ exists && (".txt" `isSuffixOf` f || ".text" `isSuffixOf` f)) files

processFile :: FilePath -> IO [Sentence]
processFile path = do
    content <- readFile path
    return $ processText content

processAndPrintFile :: FilePath -> IO ()
processAndPrintFile path = do
    sentences <- processFile path
    let sortedSentences = sortSentencesByWordCount sentences
    putStrLn $ "=== " ++ path ++ " ==="
    mapM_ (putStrLn . sentenceToString) sortedSentences
    putStrLn ""

