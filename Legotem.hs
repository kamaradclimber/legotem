import System.Environment
import Network.URI
import Legotem.Tambouille
import System.Console.GetOpt
import Legotem.Types hiding(id)
import Data.Default
import Data.Maybe

options :: [OptDescr (Options -> Options)]
options = [
  Option  "l" ["limit"]  (ReqArg (\c o -> o { limit = read c} ) "LIMIT") "max number of results",
  Option "u" ["url"]    (ReqArg (\u o -> o { url = parseURI u}) "URL")     "url to use instead of default",
  Option "o" ["output"]    (ReqArg (\u o -> o { output = u}) "FILE")     "file to store the rss",
  Option "h" ["help"]   (NoArg (\o-> o{help = True}))       "this message"
  ]
instance Default Options where 
  def = Options {
    limit = 5,
    url = parseURI "http://api.allocine.fr/rest/v3/movielist?partner=YW5kcm9pZC12M3M&filter=comingsoon&order=dateasc&format=json",
    help = False,
    output = "/dev/null"
    }

realMain :: Options -> IO ()
realMain o = 
  maybe (return ()) (blop o. download) (urlFormat o)


main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (o,_,[]) -> showHelpOrContinue options (foldl (flip id) def o)
    (_,_, errs) -> ioError (userError (concat errs ++ usageInfo header options))

header = "Usage: legotem [OPTIONS…]"


urlFormat :: Options -> Maybe URI
urlFormat o =
  parseURI $ (maybe "niet" show $ url o) ++ "&count " ++ (show $ limit o)

showHelpOrContinue :: [OptDescr (Options -> Options)]  -> Options -> IO ()
showHelpOrContinue options o =
  if help o then
    ioError $ userError $ usageInfo header options
  else 
    realMain o
