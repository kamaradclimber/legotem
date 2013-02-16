{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
module Legotem.Tambouille where

import Legotem.Types

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Data.List
import Data.Maybe

import Data.Time.Clock.POSIX
import Data.Time.Format
import Data.Time.LocalTime
import Data.Time.RFC2822
import Imm.Util ((>/>))
import Network.HTTP
import Network.URI
import System.Environment.XDG.BaseDir
import System.IO.Error
import System.Locale
import Text.Feed.Constructor
import Text.Feed.Export
import Text.Feed.Types as FT
import Text.XML.Light.Output

download :: URI -> IO String
download uri = do
  resp <- simpleHTTP ( defaultGETRequest uri)
  getResponseBody resp

blop :: IO String -> IO ()
blop is = do
  s <- is
  ordered <-  maybe (return []) createOrderedMovie (decode $ pack s  :: Maybe AllocineResponse)
  print $ map (show.  snd) ordered
  writeFile "./test" $ ppTopElement $ xmlFeed $ releaseFeed ordered


releaseFeed :: [(ZonedTime, Movie)] -> Feed
releaseFeed ms = 
  withFeedTitle "Next Movie Releases" 
  $ withFeedHome "www.allocine.fr" 
  $ withFeedDescription "Feed automatically generated to present next movies releases"
  $ withFeedItems (map createItem ms)
  $ newFeed AtomKind

createItem :: (ZonedTime, Movie) -> FT.Item
createItem (t, m) =
  withItemTitle (title m)
  $ withItemPubDate (showRFC2822 t)
  $ withItemDescription (pp m)
  $ newItem AtomKind

createOrderedMovie :: AllocineResponse -> IO [(ZonedTime, Movie)]
createOrderedMovie resp = do
   datedMovies <- mapM firstSeen ms
   return $ sort datedMovies
   where ms = movies  $ feed resp


fileNameChar :: Char -> String
fileNameChar c | isAlphaNum c = [c]
fileNameChar c | isSpace c = "-"
fileNameChar _ = ""


movieFileName :: Movie -> String
movieFileName m = 
  concatMap fileNameChar (title m)


firstSeen :: Movie -> IO (ZonedTime, Movie)
firstSeen m = do
  t <- getZonedTime
  firstSeen' t  m

firstSeen' :: ZonedTime -> Movie -> IO (ZonedTime, Movie)
firstSeen' t m = do
  p <- getUserConfigDir "legotem" >/> "state" >/> movieFileName m
  f <- getFirstSeen p
  if f == timeZero 
    then
      do 
        setFirstSeen p t
        return (t,m)
    else
        return (f,m)

setFirstSeen :: FilePath -> ZonedTime -> IO ()
setFirstSeen p d =
  writeFile p (toEpochS d)

getFirstSeen :: FilePath -> IO ZonedTime
getFirstSeen p = do
  result <- tryIOError $ readFile p
  return (either (const timeZero) fromEpochS result)


-- Time related boilerplate. This is so fucking annoying to deal with time!
timeZero :: ZonedTime
timeZero = utcToZonedTime (hoursToTimeZone 2) (posixSecondsToUTCTime 10)

fromEpochS :: String -> ZonedTime
fromEpochS s = 
  fromMaybe timeZero $ parseTime defaultTimeLocale "%s" s

toEpochS :: ZonedTime -> String
toEpochS = formatTime defaultTimeLocale "%s"

instance Ord ZonedTime where
  a <= b = zonedTimeToUTC a <= zonedTimeToUTC b 

instance Eq ZonedTime where
  a == b = zonedTimeToUTC a == zonedTimeToUTC b && zonedTimeZone a == zonedTimeZone b


pp m = intercalate "<br/>" $ map ($ m) ppMovie

ppMovie :: [Movie -> String]
ppMovie = [
   title
  , date . release
  , uri . trailer
  , synopsis
  , \m -> "Link: " ++ show (movieLink m)
  ]

movieLink :: Movie -> URI
movieLink m = fromMaybe nullURI $ movieLink' m "aco:web"

movieLink' :: Movie -> String -> Maybe URI
movieLink' m r = (parseURI . href ) =<< find (\l -> rel l== r ) (links m)
