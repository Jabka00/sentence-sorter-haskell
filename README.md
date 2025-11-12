# Sentence Sorter

Haskell program that processes text files and outputs all sentences sorted by word count in ascending order.

## Usage

1. Place text files (`.txt` or `.text`) in the project directory
2. Run: `cabal run`

Each text file is processed separately and its sentences are sorted by word count.

The program will:
- Process all text files in the current directory
- Normalize tabs and multiple spaces to single spaces
- Extract sentences from the text
- Sort sentences by word count (ascending)
- Output sorted sentences

## Test Files

Three test files are included:
- `test.txt` 
- `test1.txt` 
- `test2.txt` 

