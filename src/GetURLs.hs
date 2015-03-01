module GetURLs
(
    getURLTags,
    openURL,
    readURLFile,
) where

import Network.HTTP
import Text.HTML.TagSoup
import Data.List

urlPrefix :: String
urlPrefix = "http://www.mixesdb.com"


-- Note: does not catch exceptions, but probably should
openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)


getAttributes :: [Tag String] -> String -> [String]
getAttributes [] _ = []
getAttributes (x:xs) attrib
    | isTagOpen x = [(fromAttrib attrib x)] ++ getAttributes xs attrib
    | otherwise = getAttributes xs attrib


assignPrefix :: [String] -> [String]
assignPrefix xs = map (urlPrefix ++) xs


filterURLs :: [String] -> [String]
filterURLs xs = filter (isInfixOf "Gilles_Peterson_Worldwide") xs


getTracklistURLs :: [Tag String] -> [String]
getTracklistURLs tags =  filterURLs $ assignPrefix $ getAttributes tags "href"


getURLTags :: String -> IO [Tag String]
getURLTags = (fmap parseTags . openURL)


readURLFile :: FilePath -> IO [String]
readURLFile filePath =  do
                        inputURLs <- readFile filePath
                        urlData <- mapM getURLTags $ lines inputURLs
                        let tracklistURLs = map getTracklistURLs urlData
                        return $ concat tracklistURLs



