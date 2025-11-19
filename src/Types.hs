module Types
    ( Symbol(..)
    , TextWord(..)
    , Sentence(..)
    ) where

data Symbol = Letter Char | Digit Char | PunctuationMark Char | SpaceChar | Tab | Newline | Other Char
    deriving (Eq, Show)

data TextWord = TextWord [Symbol]
    deriving (Eq, Show)

data Sentence = Sentence [TextWord] [Symbol]
    deriving (Eq, Show)

