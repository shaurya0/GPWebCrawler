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
