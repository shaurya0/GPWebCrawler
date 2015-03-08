module GPTracklists
(
    createGPShows,
    createGPShow,
    getTracklistFromURL,
    getAirDate,
) where

import Text.HTML.TagSoup
import Data.String.Utils
import Text.Regex.Posix
import GetURLs
import GPShow
import Data.Maybe
import Data.List
import qualified Data.List.Split as Split

type Tracklist = [String]
type AirDate = (Int, Int, Int)

getOrderedList :: [Tag String] -> [Tag String]
getOrderedList tags = dropWhile (~/="<ol>") $ takeWhile (~/="</ol>") $ tail tags


getTagsAfterOrderedList :: [Tag String] -> [Tag String]
getTagsAfterOrderedList tags = dropWhile (~/="</ol>") $ tail tags


tracklistHelper :: [Tag String] -> [Tag String]
tracklistHelper [] = []
tracklistHelper tags = getOrderedList tags ++ tracklistHelper (getTagsAfterOrderedList tags)


getTracklistTags :: [Tag String] -> Maybe [Tag String]
getTracklistTags tags
    | null orderedList = Nothing
    | otherwise = Just (tracklistHelper tags)
        where orderedList = dropWhile (~/="<ol>") tags


sanitizeTracklist :: Tracklist -> Tracklist
sanitizeTracklist [] = []
sanitizeTracklist (x:xs)
    | x == "\n" = sanitizeTracklist xs
    | not (isInfixOf " - " x)  = sanitizeTracklist xs
    | otherwise = (strip x) : sanitizeTracklist xs


getTracklistFromTags :: Maybe [Tag String] -> Maybe Tracklist
getTracklistFromTags Nothing = Nothing
getTracklistFromTags (Just tracklistTags) = Just (sanitizeTracklist $ map fromTagText $ filter isTagText tracklistTags)



getTracklistFromURL :: String -> IO (Maybe Tracklist)
getTracklistFromURL tracklistURL = do
    tracklistData <- getURLTags tracklistURL
    let tags = getTracklistTags tracklistData
    let tracklist = getTracklistFromTags tags
    return tracklist


dateListToTuple :: [String] -> AirDate
dateListToTuple [year, month, day] = (read year :: Int , read month :: Int, read day :: Int)


getAirDate :: String -> Maybe AirDate
getAirDate url
        | containsDate = Just $ dateListToTuple $ Split.splitOn "-" (url =~ datePattern :: String)
        | otherwise = Nothing
            where   datePattern = "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]"
                    containsDate = url =~ datePattern :: Bool


createGPShow :: Maybe AirDate -> Maybe Tracklist -> Maybe GPShow
createGPShow _ Nothing = Nothing
createGPShow Nothing _ = Nothing
createGPShow (Just (year, month, day)) (Just tracklist) = Just (GPShow year month day tracklist)

createGPShows :: [Maybe AirDate] -> [Maybe Tracklist] -> [GPShow]
createGPShows [] [] = []
createGPShows _ [] = [] -- should throw exception here
createGPShows [] _ = [] -- should throw exception here
createGPShows (a:as) (t:ts)
                | isJust a && isJust t = fromJust (createGPShow a t) : createGPShows as ts
                | otherwise = createGPShows as ts
