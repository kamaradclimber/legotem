{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
module Legotem.Tambouille where

import Legotem.Types

import Network.URI
import Network.HTTP
import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.Char
import Imm.Util ((>/>))
import System.Environment.XDG.BaseDir
import System.IO.Error
import  Data.Time.Clock.POSIX
import Data.Time.Format
import System.Locale
import Data.List

download :: URI -> IO String
download uri = do
  resp <- simpleHTTP ( defaultGETRequest uri)
  getResponseBody resp

blop :: IO String -> IO ()
blop is = do
  s <- is
  print $ maybe s show (decode $ pack s  :: Maybe AllocineResponse)

--createOrderedMovie :: AllocineResponse -> [(POSIXTime, Movie)]
--createOrderedMovie = do
--  datedMovies <- mapM firstSeen movies
--  sort datedMovies


fileNameChar :: Char -> String
fileNameChar c | isAlphaNum c = [c]
fileNameChar c | isSpace c = "-"
fileNameChar _ = ""


movieFileName :: Movie -> String
movieFileName m = 
  concatMap fileNameChar (title m)


firstSeen :: Movie -> IO (POSIXTime, Movie)
firstSeen m = do
  t <- getPOSIXTime
  firstSeen' t  m

firstSeen' :: POSIXTime -> Movie -> IO (POSIXTime, Movie)
firstSeen' t m = do
  p <- getUserConfigDir "legotem" >/> "state" >/> movieFileName m
  f <- getFirstSeen p
  if f == timeZero then
      do 
        setFirstSeen p t
        return (t,m)
      else
        return (f,m)

setFirstSeen :: FilePath -> POSIXTime -> IO ()
setFirstSeen p d =
  writeFile p (toEpochS d)

getFirstSeen :: FilePath -> IO POSIXTime
getFirstSeen p = do
  result <- tryIOError $ readFile p
  return (either (const timeZero) fromEpochS result)


-- Time related boilerplate. This is so fucking annoying to deal with time!
timeZero :: POSIXTime
timeZero = fromIntegral (0 :: Integer)

fromEpochS :: String -> POSIXTime
fromEpochS s = 
  maybe timeZero utcTimeToPOSIXSeconds ( parseTime defaultTimeLocale "%s" s)

toEpochS :: POSIXTime -> String
toEpochS = formatTime defaultTimeLocale "%s" .posixSecondsToUTCTime
