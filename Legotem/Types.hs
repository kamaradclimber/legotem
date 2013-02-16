{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings, DeriveGeneric #-}
module Legotem.Types where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Aeson
import Data.ByteString.Lazy.Char8()
import Data.List
import GHC.Generics (Generic)
import Network.HTTP()
import Network.URI()
import qualified Data.ByteString.Lazy.Char8 as BS()

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
  release :: Release,
  id :: Int,
  synopsis :: String,
  trailer :: Trailer,
  links :: [Link]
} deriving (Eq, Ord, Show)


instance FromJSON Movie where
  parseJSON (Object v) =
    Movie <$>
    (v .: "title")     <*>
    (v .: "release")   <*>
    (v .: "code")      <*>
    (v .: "synopsisShort") <*>
    (v .: "trailer")       <*>
    (v .: "link")
  parseJSON _ = mzero

data Release = Release {
  date :: String
} deriving (Show, Eq, Ord)

instance FromJSON Release where
  parseJSON (Object v) = 
    Release <$>
    (v .: "releaseDate")
  parseJSON _ = mzero

data Trailer = Trailer {
  code :: Int,
  uri :: String
} deriving (Eq, Show, Ord)


instance FromJSON Trailer where
  parseJSON (Object v) = 
    Trailer <$>
    (v .: "code") <*>
    (v .: "href") 
  parseJSON _ = mzero

data Link = Link {
  rel ::String,
  name :: String,
  href :: String
} deriving (Show, Eq, Ord, Generic)

instance FromJSON Link


