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


dispatch :: Int -> Either String [String] -> IO()
dispatch 1 (Right (url:filePath:_)) = do
                            createGPShowFromURL url filePath
                            putStrLn $ "Wrote JSON file of GP Show from URL " ++ url ++ " to file " ++ filePath

dispatch 2 (Right (jsonFile1:jsonFile2:outputFilePath:_)) = do
                            mergeGPShowJSONFiles jsonFile1 jsonFile2 outputFilePath
                            putStrLn $ "Merged JSON files : " ++ jsonFile1 ++ ", " ++ jsonFile2 ++ " into output file " ++ outputFilePath

dispatch 3 (Right (urlFilePath:outputFilePath:_)) = do
                            createGPShowFromURLs urlFilePath outputFilePath
                            putStrLn $ "Created JSON file with shows from url file : " ++ urlFilePath ++ " into output file " ++ outputFilePath
dispatch _ (Left x) = do putStrLn x



-- todo: get rid of boilerplate
getAdditionalArgs :: Int -> IO(Either String [String] )
getAdditionalArgs 1 = do
            putStrLn "Provide URL followed by output file path"
            args <- sequence [getLine, getLine]
            let (url:filePath:_) = args
            let urlGood = N.isAbsoluteURI url
            let pred = urlGood
            if pred then return (Right args) else return (Left "predicate failed")
getAdditionalArgs 2 = do
            putStrLn "Provide filepaths for first and second JSON file, and the filepath for the output result"
            args <- sequence [getLine, getLine, getLine]
            let (jsonFile1:jsonFile2:outputFile:_) = args
            pred1 <- doesFileExist jsonFile1
            pred2 <- doesFileExist jsonFile2
            if pred1 && pred2 then return (Right args) else return (Left "predicate failed")
getAdditionalArgs 3 = do
            putStrLn "Provide URL filepath and output file path"
            args <- sequence [getLine, getLine]
            let (urlFile:outputFile:_) = args
            pred <- doesFileExist urlFile
            if pred then return (Right args) else return (Left "predicate failed")
