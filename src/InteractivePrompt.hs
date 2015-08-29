{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module InteractivePrompt where
import System.Console.CmdArgs
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad

import Data.List as L
import GetURLs
import GPTracklists
import Data.Aeson
import qualified Data.ByteString.Lazy as B

-- Fetches a multiple GP show tracklists from URLs in a file and write to file in JSON format
-- The urls in the file are assumed to have the format of 200 mixes per page from mixesdb.com
-- e.g. http://bit.ly/1wgd2MX
createGPShowFromCategoryURL :: String -> FilePath -> IO ()
createGPShowFromCategoryURL url outputFilePath = do
    urls <- readCategoryURL url
    -- let airDates = L.map getAirDate urls
    -- tracklists <- mapM getTracklistFromURL urls
    -- let gpShows = createGPShows airDates tracklists
    -- let outputResult = encode gpShows
    print "wtf"
    -- B.writeFile outputFilePath outputResult

data MyOptions = MyOptions
    {
        url :: String,
        outputFilePath :: FilePath
    }
    deriving (Data, Typeable, Show, Eq)

myProgOpts :: MyOptions
myProgOpts = MyOptions
    { url = def &= typ "URL" &= help "link to category url"
    , outputFilePath = def &= typFile &= help "path to output json file"
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
optionHandler MyOptions{..} = do
    let url_ = url
    let dir = takeDirectory outputFilePath
    dirGood <- doesDirectoryExist dir
    unless dirGood $ putStrLn ("the directory of the outputfile " ++ outputFilePath ++  ": does not exist") >> exitWith (ExitFailure 1)
    createGPShowFromCategoryURL url_ outputFilePath
