-- Find With Timestamps
-- Usage:
--  $ runghc fwt.hs '20150913 01:50:00,20150913 01:52:00'
--  $ cabal exec -- runghc fwt.hs '20150913 01:50:00,20150913 01:52:00'  # for cabal sandbox

import Data.List.Split (splitOn)
import qualified Data.Time.Format as F
import Data.Time.LocalTime (LocalTime)
import System.Environment (getArgs)
import System.Process (callProcess)

main :: IO ()
main =
  getArgs >>= findWithTimestamps . handleArgs

-- TODO support specifying a start time and a duration (in minute?)
-- TODO path param
-- TODO check params
-- TODO locale
-- TODO better error report
-- TODO also support atime and ctime
handleArgs :: [String] -> (String, String)
handleArgs args =
  let
    locale                 =  F.defaultTimeLocale
    timeFormat             =  "%Y%m%d %H:%M:%S"
    parseTime              :: String -> Maybe LocalTime
    parseTime              =  F.parseTimeM False locale timeFormat
    [startTimeM, endTimeM] =  (map parseTime . splitOn "," . head) args
    showTime               :: LocalTime -> String
    showTime               =  F.formatTime locale timeFormat
  in
    case (startTimeM, endTimeM) of
      (Just startTime, Just endTime) -> (showTime startTime, showTime endTime)
      _                              -> error "Failed to parse timestamps."

findWithTimestamps                      :: (String, String) -> IO ()
findWithTimestamps (startTime, endTime) =
  callProcess "find" ["-newermt", startTime, "!", "-newermt", endTime]
