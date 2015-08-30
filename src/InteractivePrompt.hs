{-# LANGUAGE DeriveDataTypeable #-}

module InteractivePrompt where
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Control.Applicative
import GetURLs
import GPTracklists
import Data.Aeson
import qualified Data.ByteString.Lazy as B
import Control.Concurrent.Async

-- Fetches a multiple GP show tracklists from URLs in a file and write to file in JSON format
-- The urls in the file are assumed to have the format of 200 mixes per page from mixesdb.com
-- e.g. http://www.mixesdb.com/db/index.php?title=Category%3AGilles+Peterson&pagefrom=1980

createGPShowFromCategoryURL :: String -> FilePath -> IO ()
createGPShowFromCategoryURL url_ outputFilePath_ = do
    urls <- readCategoryURL url_
    let airDates = map getAirDate urls
    tracklists <- mapConcurrently getTracklistFromURL urls
    let gpShows = createGPShows airDates tracklists
    let outputResult = encode gpShows
    B.writeFile outputFilePath_ outputResult

data MyOptions = MyOptions
    {
        url :: Maybe String,
        outputFilePath :: Maybe FilePath
    }
    deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions
    {
        url = Nothing &= typ "URL" &= help "link to category url"
        , outputFilePath = Nothing &= typ "file path" &= help "path to output json file"
    }


getOpts :: IO MyOptions
getOpts  = cmdArgs $ myProgOpts
    &= summary _PROGRAM_INFO
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME


_PROGRAM_NAME :: String
_PROGRAM_NAME = "Gilles Peterson Web Crawler"
_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.1"
_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT :: String
_PROGRAM_ABOUT = "Scrapes tracklists of the yearly category urls of Gilles Peterson shows from mixesdb.com, e.g http://bit.ly/1wgd2MX and outputs the result to a JSON file"


optionHandler :: MyOptions -> IO()
optionHandler (MyOptions Nothing _) = print "URL required"
optionHandler (MyOptions _ Nothing) = print "Filepath required"
optionHandler (MyOptions (Just url_) (Just outputFilePath_)) = do
    let dir = takeDirectory outputFilePath_
    let filePathGood = all ($ outputFilePath_) [isAbsolute, isValid, hasExtension]
    fileInputGood <- pure (&&) <*> pure filePathGood <*> doesDirectoryExist dir
    unless fileInputGood $ putStrLn ("the path for the outputfile " ++ outputFilePath_ ++  ": is invalid") >> exitWith (ExitFailure 1)
    let urlGood = not (null url_)
    unless urlGood $ putStrLn "empty url" >> exitWith (ExitFailure 1)
    createGPShowFromCategoryURL url_ outputFilePath_
