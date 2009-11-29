module Main where

import Char (ord)
import Control.Monad
import Control.Monad.Loops
import System.Hardware.Serialport

main = do s <- openSerial "/dev/ttyUSB0" defaultSerialSettings { baudRate = B2400 }
          sendChar s 'V'
          response <- unfoldM (recvChar s)
          print $ parse $ map ord response
          closeSerial s

showAA :: [(Int,Int,Int)] -> String
showAA = concat . zipWith format [1..]
    where format n aa = " Data " ++ show n ++ ":    " ++ show aa ++ "\n"

data Sleep = Sleep {
      date :: (Int,Int),
      window :: Int,
      toBed :: (Int,Int),
      alarm :: (Int,Int),
      almostAwakes :: [(Int,Int,Int)]
}

instance Show Sleep where
    show s = "Date:                  " ++ show (date s) ++ "\n\
             \To Bed:                " ++ show (toBed s) ++ "\n\
             \Alarm Time:            " ++ show (alarm s) ++ "\n\
             \Effective Alarm Time:  " ++ (show $ last $ almostAwakes s) ++ "\n\
             \Window:                " ++ show (window s) ++ "\n\
             \\n\
             \Awake moments (" ++ show (length (almostAwakes s)) ++ "):\n" ++
             showAA (almostAwakes s)

parse :: [Int] -> Sleep
parse lst = let ([_,month,day,_,window,toBed0,toBed1,
                  alarm0,alarm1,cntData],rest) = splitAt 10 lst
            in Sleep { date         = (month,day),
                       window       = window,
                       toBed        = (toBed0,toBed1),
                       alarm        = (alarm0,alarm1),
                       almostAwakes = parseAlmostAwakes cntData rest
                     }

parseAlmostAwakes :: Int -> [Int] -> [(Int,Int,Int)]
parseAlmostAwakes count = map three . take count . groupN 3

three :: [Int] -> (Int,Int,Int)
three [a,b,c] = (a,b,c)

groupN :: Int -> [a] -> [[a]]
groupN n []  = []
groupN n lst = take n lst : groupN n (drop n lst)