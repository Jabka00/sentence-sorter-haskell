module Sorter
    ( sortSentencesByWordCount
    ) where

import Data.List (sortOn)
import Types

extractWordsFromSentence :: Sentence -> [TextWord]
extractWordsFromSentence (Sentence words _) = words

sentenceWordCount :: Sentence -> Int
sentenceWordCount = length . extractWordsFromSentence

sortSentencesByWordCount :: [Sentence] -> [Sentence]
sortSentencesByWordCount = sortOn sentenceWordCount

