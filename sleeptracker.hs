module Main where

import Char (ord)
import Control.Monad
import Control.Monad.Loops (unfoldM)
import Data.List (intersperse)
import System.Hardware.Serialport
import Text.Printf

main = do s <- openSerial "/dev/ttyUSB0" defaultSerialSettings { baudRate = B2400 }
          sendChar s 'V'
          response <- unfoldM (fmap (fmap ord) (recvChar s))
          unless (checksumIsCorrect response) (error "Checksum Error.")
          print $ parse response
          closeSerial s

data Date = Date {
      day :: Int,
      month :: Int
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
      almostAwakes :: [LongTime]
}

data TimeDiff = TimeDiff {
      tdSeconds :: Int
}

instance Show Date where
    show (Date day month) = printf "%02d.%02d." day month

instance Show LongTime where
    show (LongTime h m s) = printf "%02d:%02d:%02d" h m s

instance Show ShortTime where
    show (ShortTime hour minute) = printf "%02d:%02d" hour minute

instance Show TimeDiff where
    show (TimeDiff seconds) = printf "%2d minutes" (seconds `div` 60)

instance Show Sleep where
    show s = "Date:                  " ++ show (date s) ++ "\n\
             \To Bed:                " ++ show (toBed s) ++ "\n\
             \Alarm Time:            " ++ show (alarm s) ++ "\n\
             \Effective Alarm Time:  " ++ (show $ last $ almostAwakes s) ++ "\n\
             \Window:                " ++ show (window s) ++ "\n\
             \\n\
             \Awake moments (" ++ show (length (almostAwakes s)) ++ "):\n" ++
             showAlmostAwakes (almostAwakes s)

parse :: [Int] -> Sleep
parse lst = let ([_,month,day,_,window,toBed0,toBed1,alarm0,alarm1,cntData],rest) = splitAt 10 lst
            in Sleep { date         = Date day month,
                       window       = window,
                       toBed        = ShortTime toBed0 toBed1,
                       alarm        = ShortTime alarm0 alarm1,
                       almostAwakes = parseAlmostAwakes cntData rest
                     }

parseAlmostAwakes :: Int -> [Int] -> [LongTime]
parseAlmostAwakes count = map (\[h,m,s] -> LongTime h m s) . take count . groupN 3

groupN :: Int -> [a] -> [[a]]
groupN n []  = []
groupN n lst = take n lst : groupN n (drop n lst)

checksumIsCorrect :: [Int] -> Bool
checksumIsCorrect lst = findChecksum lst == computeChecksum lst
    where findChecksum :: [Int] -> Int
          findChecksum lst = lst !! (length lst - 2)

          computeChecksum :: [Int] -> Int
          computeChecksum = flip mod 256 . sum . drop 1 . dropLast 2

          dropLast :: Int -> [a] -> [a]
          dropLast n = reverse . drop n . reverse

seconds :: LongTime -> Int
seconds (LongTime h m s) = h * 60 * 60 +
                           m * 60 +
                           s

timeDiff :: LongTime -> LongTime -> TimeDiff
timeDiff a b = TimeDiff $ foldl1 diffSeconds $ map seconds [a,b]

diffSeconds :: Int -> Int -> Int
diffSeconds a b | a < b     = b - a
                | otherwise = toMidnight + b
    where toMidnight = midnight - a
          midnight   = 24 * 60 * 60

diffs :: (a -> a -> b) -> [a] -> [b]
diffs f []       = []
diffs f [x]      = []
diffs f (x:y:xs) = f x y : diffs f (y:xs)

showAlmostAwakes :: [LongTime] -> String
showAlmostAwakes lst = concat $ intersperse "\n" $ zipWith3 format [1..] lst sleeps
    where format n t d = printf "Data %2d:  %s   %s slept" (n :: Int) (show t) (show d)
          sleeps = TimeDiff 0 : diffs timeDiff lst