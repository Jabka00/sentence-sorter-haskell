module Main where

import FileProcessor

main :: IO ()
main = do
    textFiles <- findTextFiles
    if null textFiles
        then putStrLn "No text files found in current directory"
        else mapM_ processAndPrintFile textFiles
