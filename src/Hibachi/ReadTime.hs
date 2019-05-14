module Hibachi.ReadTime
    ( ReadTime(..)
    , addTime
    , minutesReadTime
    )
    where

import           Data.Time
import           Data.Time.Clock
import           Data.Time.Format

instance Eq ZonedTime where
    a == b = (zonedTimeToUTC a) == (zonedTimeToUTC b)
instance Ord ZonedTime where
    compare a b = compare (zonedTimeToUTC a) (zonedTimeToUTC b)

data ReadTime = ReadTime
              { numWords  :: Int
              , numImages :: Int
              }
    deriving (Eq, Ord)
instance Show ReadTime where
    show r@(ReadTime w i) =
        let m = minutesReadTime r in
            (if m <= 1 -- Our algorithm would read "0/1 minutes read"
                then "1 minute read ("
                else (show m) ++ " minutes read (")
            ++ (show w) ++ " words" ++
            -- Don't mention images if there are none
            (if i /= 0 then " and " ++ (show i) ++ "images)" else ")")

addTime :: ReadTime -> ReadTime -> ReadTime
addTime (ReadTime mw mi) (ReadTime nw ni) = ReadTime (mw + nw) (mi + ni)

minutesReadTime :: ReadTime -> Int
-- Read time (in minutes) is the number of words `div` the avg number of WPM (275)
-- plus
-- Time for looking at pictures. Medium calculates 12 seconds for the
-- first, 11 for the second, 10 for the third picture ... (`reverse
-- [3..12]`) and 3 seconds for every picture after the 10th (repeat 3)
minutesReadTime (ReadTime w i) = (w `div` 275) + ((sum $ take i $ reverse [3..12] ++ repeat 3) `div` 60)
