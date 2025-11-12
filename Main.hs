module Main where

import System.Directory
import System.IO
import Data.Char hiding (Space, isSeparator)
import Data.List
import Control.Monad

data Symbol = Letter Char | Digit Char | PunctuationMark Char | SpaceChar | Tab | Newline | Other Char
    deriving (Eq, Show)

data TextWord = TextWord [Symbol]
    deriving (Eq, Show)

data Sentence = Sentence [TextWord] [Symbol]
    deriving (Eq, Show)

normalizeSpaces :: String -> String
normalizeSpaces = unwords . words . map replaceSpecial
    where replaceSpecial '\t' = ' '
          replaceSpecial '\n' = ' '
          replaceSpecial c = c

charToSymbol :: Char -> Symbol
charToSymbol c
    | isLetter c = Letter c
    | isDigit c = Digit c
    | c `elem` ".,!?;:()[]{}'\"-" = PunctuationMark c
    | c == ' ' = SpaceChar
    | c == '\t' = Tab
    | c == '\n' = Newline
    | otherwise = Other c

stringToSymbols :: String -> [Symbol]
stringToSymbols = map charToSymbol

isWordSymbol :: Symbol -> Bool
isWordSymbol (Letter _) = True
isWordSymbol (Digit _) = True
isWordSymbol _ = False

isPunctuationSymbol :: Symbol -> Bool
isPunctuationSymbol (PunctuationMark _) = True
isPunctuationSymbol _ = False

isSymbolSeparator :: Symbol -> Bool
isSymbolSeparator SpaceChar = True
isSymbolSeparator Tab = True
isSymbolSeparator _ = False

isSentenceEnder :: Symbol -> Bool
isSentenceEnder (PunctuationMark '.') = True
isSentenceEnder (PunctuationMark '!') = True
isSentenceEnder (PunctuationMark '?') = True
isSentenceEnder Newline = True
isSentenceEnder _ = False

symbolsToWords :: [Symbol] -> [TextWord]
symbolsToWords [] = []
symbolsToWords symbols = 
    let (wordSymbols, rest) = span isWordSymbol symbols
        word = if null wordSymbols then [] else [TextWord wordSymbols]
        nextSymbols = dropWhile (\s -> not (isWordSymbol s)) rest
    in word ++ symbolsToWords nextSymbols

extractWordsFromSentence :: Sentence -> [TextWord]
extractWordsFromSentence (Sentence words _) = words

sentenceWordCount :: Sentence -> Int
sentenceWordCount = length . extractWordsFromSentence

splitIntoSentences :: [Symbol] -> [Sentence]
splitIntoSentences [] = []
splitIntoSentences symbols =
    let (beforeEnd, endAndAfter) = break isSentenceEnder symbols
        words = symbolsToWords beforeEnd
        (endSymbol, afterEnd) = case endAndAfter of
            [] -> ([], [])
            (Newline:rest) -> ([], rest)
            (e:rest) -> ([e], rest)
        rest = dropWhile (\s -> isSymbolSeparator s || s == Newline) afterEnd
        sentence = if null words then [] else [Sentence words endSymbol]
    in sentence ++ splitIntoSentences rest

processText :: String -> [Sentence]
processText text =
    let symbols = stringToSymbols text
        sentences = splitIntoSentences symbols
    in map normalizeSentence sentences
    where
        normalizeSentence (Sentence words endSyms) =
            let normalizedWords = map normalizeWord words
            in Sentence normalizedWords endSyms
        normalizeWord (TextWord syms) =
            TextWord (filter (not . isSymbolSeparator) syms)

sentenceToString :: Sentence -> String
sentenceToString (Sentence words endSymbols) =
    let wordStrings = map (\(TextWord syms) -> map symbolToChar syms) words
        sentenceText = unwords wordStrings
        endText = map symbolToChar endSymbols
    in sentenceText ++ endText

symbolToChar :: Symbol -> Char
symbolToChar (Letter c) = c
symbolToChar (Digit c) = c
symbolToChar (PunctuationMark c) = c
symbolToChar SpaceChar = ' '
symbolToChar Tab = ' '
symbolToChar Newline = ' '
symbolToChar (Other c) = c

processFile :: FilePath -> IO [Sentence]
processFile path = do
    content <- readFile path
    return $ processText content

main :: IO ()
main = do
    files <- listDirectory "."
    textFiles <- filterM (\f -> do
        exists <- doesFileExist f
        return $ exists && (".txt" `isSuffixOf` f || ".text" `isSuffixOf` f)) files
    if null textFiles
        then putStrLn "No text files found in current directory"
        else do
            mapM_ processAndPrintFile textFiles

processAndPrintFile :: FilePath -> IO ()
processAndPrintFile path = do
    sentences <- processFile path
    let sortedSentences = sortOn sentenceWordCount sentences
    putStrLn $ "=== " ++ path ++ " ==="
    mapM_ (putStrLn . sentenceToString) sortedSentences
    putStrLn ""

