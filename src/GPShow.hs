{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}

module GPShow
(
    GPShow(..),
) where

import Data.Aeson
import GHC.Generics


data GPShow =
    GPShow {  airDateYear :: Int
            , airDateMonth :: Int
            , airDateDay :: Int
            , showTracklist :: [String]
            } deriving (Show, Ord, Generic)

instance Eq GPShow where
    (GPShow y1 m1 d1 _) == (GPShow y2 m2 d2 _) = (y1 == y2) && (m1 == m2 ) && (d1 == d2)

instance FromJSON GPShow
instance ToJSON GPShow

toGPShowObject :: GPShow -> Object
toGPShowObject show = undefined

gpShowListToArray :: [GPShow] -> Array
gpShowListToArray shows = Array $ map toGPShowObject shows

-- instance FromJSON GPShow where
--  parseJSON (Object v) =
--     GPShow <$> v .: "year"
--            <*> v .: "month"
--            <*> v .: "day"
--            <*> v .: "tracklist"
--  parseJSON _ = mzero


-- instance ToJSON GPShow where
--     toJSON (GPShow year month day tracklist) =
--         object[     "year" .= year
--                 ,   "month" .= month
--                 ,   "day" .= day
--                 ,   "tracklist" .= tracklist ]
