{-# LANGUAGE OverloadedStrings #-}

module GPShow
(
    GPShow(..),
    jsonFile,
) where

import Control.Applicative
import Control.Monad
import Data.Aeson

jsonFile :: FilePath
jsonFile = "/home/shaurya/Development/GPcrawler/data/gpshow.json"


data GPShow =
    GPShow {  airDateYear :: Int
            , airDateMonth :: Int
            , airDateDay :: Int
            , showTracklist :: [String]
            } deriving (Show)

instance FromJSON GPShow where
 parseJSON (Object v) =
    GPShow <$> v .: "year"
           <*> v .: "month"
           <*> v .: "day"
           <*> v .: "tracklist"
 parseJSON _ = mzero


instance ToJSON GPShow where
    toJSON (GPShow year month day tracklist) =
        object[     "year" .= year
                ,   "month" .= month
                ,   "day" .= day
                ,   "tracklist" .= tracklist ]
