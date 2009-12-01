module Main where

import Char (ord)
import Control.Monad
import Control.Monad.Loops (unfoldM)
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

instance Show Date where
    show (Date day month) = printf "%02d.%02d." day month

data ShortTime = ShortTime {
      stHour :: Int,
      stMinute :: Int
    }

instance Show ShortTime where
    show (ShortTime hour minute) = printf "%02d:%02d" hour minute

data LongTime = LongTime {
      ltHour :: Int,
      ltMinute :: Int,
      ltSecond :: Int
}

instance Show LongTime where
    show (LongTime hour minute second) = printf "%02d:%02d:%02d" hour minute second

data Sleep = Sleep {
      date :: Date,
      window :: Int,
      toBed :: ShortTime,
      alarm :: ShortTime,
      almostAwakes :: [LongTime]
}

instance Show Sleep where
    show s = "Date:                  " ++ show (date s) ++ "\n\
             \To Bed:                " ++ show (toBed s) ++ "\n\
             \Alarm Time:            " ++ show (alarm s) ++ "\n\
             \Effective Alarm Time:  " ++ (show $ last $ almostAwakes s) ++ "\n\
             \Window:                " ++ show (window s) ++ "\n\
             \\n\
             \Awake moments (" ++ show (length (almostAwakes s)) ++ "):\n" ++
             showAlmostAwakes (almostAwakes s)

showAlmostAwakes :: [LongTime] -> String
showAlmostAwakes = concat . zipWith format [1..]
    where format n aa = " Data " ++ show n ++ ":    " ++ show aa ++ "\n"

parse :: [Int] -> Sleep
parse lst = let ([_,month,day,_,window,toBed0,toBed1,alarm0,alarm1,cntData],rest) = splitAt 10 lst
            in Sleep { date         = Date day month,
                       window       = window,
                       toBed        = ShortTime toBed0 toBed1,
                       alarm        = ShortTime alarm0 alarm1,
                       almostAwakes = parseAlmostAwakes cntData rest
                     }

parseAlmostAwakes :: Int -> [Int] -> [LongTime]
parseAlmostAwakes count = map (\(a,b,c) -> LongTime a b c) . map three . take count . groupN 3

three :: [Int] -> (Int,Int,Int)
three [a,b,c] = (a,b,c)

groupN :: Int -> [a] -> [[a]]
groupN n []  = []
groupN n lst = take n lst : groupN n (drop n lst)

checksumIsCorrect :: [Int] -> Bool
checksumIsCorrect lst = findChecksum lst == computeChecksum lst

computeChecksum :: [Int] -> Int
computeChecksum = flip mod 256 . sum . drop 1 . dropLast 2

findChecksum :: [Int] -> Int
findChecksum lst = lst !! (length lst - 2)

dropLast :: Int -> [a] -> [a]
dropLast n str = take (length str - n) str