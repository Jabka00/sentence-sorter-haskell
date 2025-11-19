module Parser
    ( processText
    , sentenceToString
    ) where

import Data.Char (isLetter, isDigit)
import Types

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

symbolToChar :: Symbol -> Char
symbolToChar (Letter c) = c
symbolToChar (Digit c) = c
symbolToChar (PunctuationMark c) = c
symbolToChar SpaceChar = ' '
symbolToChar Tab = ' '
symbolToChar Newline = ' '
symbolToChar (Other c) = c

sentenceToString :: Sentence -> String
sentenceToString (Sentence words endSymbols) =
    let wordStrings = map (\(TextWord syms) -> map symbolToChar syms) words
        sentenceText = unwords wordStrings
        endText = map symbolToChar endSymbols
    in sentenceText ++ endText

