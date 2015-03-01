module InteractivePrompt where


import System.Environment
import System.Directory
import System.FilePath
import Data.List
import Network.URI as N
import GetURLs
import Text.Read
import GPTracklists
import GPShow
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S


urlsBasePath :: FilePath
urlsBasePath = "/home/shaurya/Development/GPcrawler/data/"


-- Fetches a single GP show tracklist from an URL and writes to file in JSON format
createGPShowFromURL :: String -> FilePath -> IO ()
createGPShowFromURL url filePath = do
   let airDate = getAirDate url
   tracklist <- getTracklistFromURL url
   let gpShow = createGPShow airDate tracklist
   case gpShow of  (Just x) -> B.writeFile filePath $ encode gpShow
                   otherwise -> putStrLn "failed to write output"




-- Fetches a multiple GP show tracklists from URLs in a file and write to file in JSON format
createGPShowFromURLs :: FilePath -> FilePath -> IO [()]
createGPShowFromURLs urlFilePath outputFilePath = do
    urls <- readURLFile urlFilePath
    let airDates = map getAirDate urls
    tracklists <- mapM getTracklistFromURL urls
    let gpShows = createGPShows airDates tracklists
    mapM (B.writeFile outputFilePath) $ map encode gpShows



-- Note: this function is not very efficient
mergeGPShowJSONFiles :: FilePath -> FilePath -> FilePath -> IO [()]
mergeGPShowJSONFiles jsonFile1 jsonFile2 outputFilePath = do
    gpShows1 <- readGPShowJSONFile jsonFile1
    gpShows2 <- readGPShowJSONFile jsonFile2
    mapM (B.writeFile outputFilePath) $ map encode [gpShows1 ++ gpShows2]


readGPShowJSONFile :: FilePath -> IO([GPShow])
readGPShowJSONFile fileName = do
    contents <- B.readFile fileName
    let gpShows = decode contents :: Maybe [GPShow]
    return $ fromJust gpShows

validateInput :: String -> Maybe Int
validateInput args
        | isJust x && elem (fromJust x) [1..3] = x
        | otherwise = Nothing
            where x = readMaybe args :: Maybe Int

printChoice :: Maybe Int -> IO ()
printChoice (Just x) = do
    putStrLn $ "Selected option " ++ show x
    args <- getAdditionalArgs x
    dispatch x args
printChoice Nothing = putStrLn "Input argument invalid"


dispatch :: Int -> Maybe [String] -> IO()
dispatch 1 (Just (url:filePath:_)) = do
                            createGPShowFromURL url filePath
                            putStrLn $ "Wrote JSON file of GP Show from URL " ++ url ++ " to file " ++ filePath

dispatch 2 (Just (jsonFile1:jsonFile2:outputFilePath:_)) = do
                            mergeGPShowJSONFiles jsonFile1 jsonFile2 outputFilePath
                            putStrLn $ "Merged JSON files : " ++ jsonFile1 ++ ", " ++ jsonFile2 ++ " into output file " ++ outputFilePath

dispatch 3 (Just (urlFilePath:outputFilePath:_)) = do
                            createGPShowFromURLs urlFilePath outputFilePath
                            putStrLn $ "Created JSON file with shows from url file : " ++ urlFilePath ++ " into output file " ++ outputFilePath
dispatch _ Nothing = do putStrLn "No operation performed"


data ArgType = InputFile | URL | OutputFile

validateArg :: String -> ArgType -> IO(Bool)
validateArg filePath InputFile = do
                fileExists <- doesFileExist filePath
                if fileExists then (putStrLn $ "Input file : " ++ filePath) else (putStrLn $ "Error : " ++ filePath ++ " does not exist")
                return fileExists
validateArg url URL = do
                let urlGood = N.isAbsoluteURI url
                if urlGood then (putStrLn $ "Input URL : " ++ url) else (putStrLn $ "Error : " ++ url ++ " is invalid")
                return urlGood
validateArg filePath OutputFile = do
                let dir = takeDirectory filePath
                dirGood <- doesDirectoryExist dir
                if dirGood then (putStrLn $ "Output file : " ++ filePath) else (putStrLn $ "Error : " ++ filePath ++ " is invalid")
                return dirGood




-- todo: get rid of boilerplate
getAdditionalArgs :: Int -> IO(Maybe [String])
getAdditionalArgs 1 = do
            putStrLn "Provide URL"
            url <- getLine
            putStrLn "Provide output file path"
            outputFilePath <- getLine
            let args = [url, outputFilePath]
            validArgs <- zipWithM validateArg args [URL, OutputFile]
            let predicate = and validArgs
            if predicate then return (Just args) else return Nothing
getAdditionalArgs 2 = do
            putStrLn "Provide filepath for the first JSON file"
            jsonFile1 <- getLine
            putStrLn "Provide filepath for the second JSON file"
            jsonFile2 <- getLine
            putStrLn "Provide output file path"
            outputFilePath <- getLine
            let args = [jsonFile1, jsonFile2, outputFilePath]
            validArgs <- zipWithM validateArg args [InputFile, InputFile, OutputFile]
            let predicate = and validArgs
            if predicate then return (Just args) else return Nothing
getAdditionalArgs 3 = do
            putStrLn "Provide filepath for URL file"
            urlFile <- getLine
            putStrLn "Provide output file path"
            outputFilePath <- getLine
            let args = [urlFile, outputFilePath]
            validArgs <- zipWithM validateArg args [InputFile, OutputFile]
            let predicate = and validArgs
            if predicate then return (Just args) else return Nothing
