import System.Environment
import Network.URI
import Legotem.Tambouille

main :: IO ()
main = getArgs >>= realMain . head


realMain :: String -> IO ()
realMain s = 
  maybe (return ()) (blop . download) (parseURI s)

