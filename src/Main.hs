module Main where

import InteractivePrompt


mainInputOptions :: String
mainInputOptions = "Options:\nSelect 1 for writing a single URL to file\nSelect 2 for merging two JSON files\nSelect 3 for writing a category URL to JSON file"


main :: IO()
main = do
        putStrLn mainInputOptions
        inputArg <- getLine
        let inputOption = validateInput inputArg
        printChoice inputOption
