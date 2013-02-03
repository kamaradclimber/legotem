{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
module Legotem.Types where

import Network.URI
import Network.HTTP
import Data.Aeson
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.ByteString.Lazy.Char8

data AllocineResponse = AllocineResponse {
  feed :: AllocineFeed
} deriving (Show,Eq)

instance FromJSON AllocineResponse where
  parseJSON (Object v) = 
    AllocineResponse <$>
    (v .: "feed")
  parseJSON _ = mzero

data AllocineFeed = AllocineFeed {
  updated :: String,
  movies  :: [Movie]
} deriving (Show,Eq)

instance FromJSON AllocineFeed where
  parseJSON (Object v) =
    AllocineFeed <$>
    (v .: "updated") <*>
    (v .: "movie")
  parseJSON _ = mzero

data Movie = Movie {
  title :: String,
  release :: Release
} deriving (Show, Eq)

instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$>
    (v .: "title")     <*>
    (v .: "release")
  parseJSON _ = mzero

data Release = Release {
  date :: String
} deriving (Show, Eq)

instance FromJSON Release where
  parseJSON (Object v) = 
    Release <$>
    (v .: "releaseDate")
  parseJSON _ = mzero
