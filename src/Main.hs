module Main where

import InteractivePrompt
import System.Environment

main :: IO()
main = do
    args <- getArgs
    opts <- (if length args /= 2 then withArgs ["--help"] else id) getOpts
    optionHandler opts
