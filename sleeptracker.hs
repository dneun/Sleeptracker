module Main where

import Char (ord)
import Control.Monad
import Control.Monad.Loops (unfoldM)
import Data.List (intersperse)
import System.Hardware.Serialport
import Text.Printf
import Time hiding (TimeDiff)

main = do s <- openSerial "/dev/ttyUSB0" defaultSerialSettings { baudRate = B2400 }
          sendChar s 'V'
          response <- unfoldM (fmap (fmap ord) (recvChar s))
          unless (checksumIsCorrect response) (error "Checksum Error.")
          ct <- (getClockTime >>= toCalendarTime)
          print $ parse response (ctYear ct)
          closeSerial s

data Date = Date {
      day :: Int,
      month :: Int,
      year :: Int
}

data ShortTime = ShortTime {
      stHour :: Int,
      stMinute :: Int
}

data LongTime = LongTime {
      ltHour :: Int,
      ltMinute :: Int,
      ltSecond :: Int
}

data Sleep = Sleep {
      date :: Date,
      window :: Int,
      toBed :: ShortTime,
      alarm :: ShortTime,
      dataA :: DataA,
      almostAwakes :: [LongTime]
}

data TimeDiff = TimeDiff {
      tdSeconds :: Int
}

data DataA = DataA {
      daSeconds :: Int
}

instance Show Date where
    show (Date day month year) = printf "%02d.%02d.%04d" day month year

instance Show LongTime where
    show (LongTime h m s) = printf "%02d:%02d:%02d" h m s

instance Show ShortTime where
    show (ShortTime hour minute) = printf "%02d:%02d" hour minute

instance Show TimeDiff where
    show (TimeDiff seconds) = printf "%2d min" (seconds `div` 60)

instance Show DataA where
    show (DataA seconds) = printf "%02d:%02d min" (seconds `div` 60) (seconds `mod` 60)

instance Show Sleep where
    show s = "\n\
             \Date:                  " ++ show (date s) ++ "\n\
             \To Bed:                " ++ show (toBed s) ++ "\n\
             \Alarm Time:            " ++ show (alarm s) ++ "\n\
             \Effective Alarm Time:  " ++ (show $ last $ almostAwakes s) ++ "\n\
             \Window:                " ++ show (window s) ++ " min\n\
             \Data A (Clock):        " ++ show (dataA s) ++ "\n\
             \Data A (Calculated):   " ++ show (computeDataA s) ++ "\n\
             \\n\
             \Awake moments (" ++ show (length (almostAwakes s)) ++ "):\n" ++
             showAlmostAwakes (toBed s) (almostAwakes s) ++ "\n"

parse :: [Int] -> Int -> Sleep
parse lst year = 
    let ([_,month,day,_,window,toBed0,toBed1,alarm0,alarm1,cntData],rest) = splitAt 10 lst
        (left,right) = splitAt (cntData * 3) rest
    in Sleep { date         = Date day month year,
               window       = window,
               toBed        = ShortTime toBed0 toBed1,
               alarm        = ShortTime alarm0 alarm1,
               almostAwakes = map3 LongTime left,
               dataA        = parseDataA right
             }

parseDataA :: [Int] -> DataA
parseDataA = DataA . sum . zipWith (*) [1,0xff]

computeDataA :: Sleep -> DataA
computeDataA sleep = 
    DataA $ (`div` count) $ foldl1 diffSeconds $ map seconds [expand (toBed sleep),awake]
        where expand (ShortTime h m) = LongTime h m 0
              count = length (almostAwakes sleep)
              awake = last (almostAwakes sleep)

map3 :: (a -> a -> a -> b) -> [a] -> [b]
map3 f []         = []
map3 f (x:y:z:xs) = f x y z : map3 f xs

checksumIsCorrect :: [Int] -> Bool
checksumIsCorrect lst = findChecksum lst == computeChecksum lst
    where findChecksum :: [Int] -> Int
          findChecksum lst = lst !! (length lst - 2)

          computeChecksum :: [Int] -> Int
          computeChecksum = flip mod 256 . sum . drop 1 . dropLast 2

          dropLast :: Int -> [a] -> [a]
          dropLast n = reverse . drop n . reverse

showAlmostAwakes :: ShortTime -> [LongTime] -> String
showAlmostAwakes toBed lst = concat $ intersperse "\n" $ zipWith3 format [1..] lst sleeps
    where format n t d = printf " Data %2d: %s (slept:  %s)" (n :: Int) (show t) (show d)
          sleeps = diffs timeDiff (expand toBed : lst)
          expand (ShortTime h m) = LongTime h m 0

timeDiff :: LongTime -> LongTime -> TimeDiff
timeDiff a b = TimeDiff $ foldl1 diffSeconds $ map seconds [a,b]

seconds :: LongTime -> Int
seconds (LongTime h m s) = h * 60 * 60 + m * 60 + s

diffSeconds :: Int -> Int -> Int
diffSeconds a b | a < b     = b - a
                | otherwise = toMidnight + b
    where toMidnight = midnight - a
          midnight   = 24 * 60 * 60

diffs :: (a -> a -> b) -> [a] -> [b]
diffs f []       = []
diffs f [x]      = []
diffs f (x:y:xs) = f x y : diffs f (y:xs)