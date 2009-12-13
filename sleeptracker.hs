module Main where

import Char (ord,toUpper)
import Control.Applicative ((<$>))
import Control.Monad (when,unless)
import Control.Monad.Loops (unfoldM)
import Data.List (intersperse)
import System.Hardware.Serialport
import Text.Printf (printf)
import Text.ParserCombinators.Parsec (GenParser,ParseError,count,tokenPrim,parse,parseTest)
import Time (getClockTime,toCalendarTime,ctYear)
import System (exitWith)
import System.Environment (getArgs)
import System.Process (runCommand,waitForProcess)

data Output = Text
            | Csv
            | Browser
            | Xml 
              deriving (Read)

main = do args <- getArgs
          let (format,device) = parseArgs args
          s <- openSerial device defaultSerialSettings { baudRate = B2400 }
          sendChar s 'V'
          response <- unfoldM $ fmap ord <$> recvChar s
          when (length response < 15) (error short)
          unless (checksumIsCorrect response) (error "Checksum Error.")
          year <- ctYear <$> (toCalendarTime =<< getClockTime)
          sleep <- toIO $ parse (sleepParser year) "" response
          output format sleep
          closeSerial s
    where
      parseArgs :: [String] -> (Output,FilePath)
      parseArgs [device]        = (Browser,device)
      parseArgs [output,device] = (read $ capitalise output,device)
      parseArgs _               = error help

      output :: Output -> Sleep -> IO ()
      output Text    = putStrLn . show
      output Csv     = putStrLn . csv
      output Browser = execute . browser
      output Xml     = putStrLn . xml

      execute :: String -> IO ()
      execute s = putStrLn s >> runCommand s >>= waitForProcess >>= exitWith

      help :: String
      help = "Usage: sleeptracker [format] device\n\
             \format:    browser(default), text, csv, xml\n"

      toIO :: Either ParseError a -> IO a
      toIO = either (error.show) return

      short = "Error while reading from Sleeptracker!\n\
              \Watch showing DATA screen?\n"

      capitalise :: String -> String
      capitalise s = (toUpper $ head s) : tail s

data Date = Date { day, month, year :: Int }

data ShortTime = ShortTime { stHour, stMinute :: Int }

data LongTime = LongTime { ltHour, ltMinute, ltSecond :: Int }

data Sleep = Sleep {
      date :: Date,
      window :: Window,
      toBed :: ShortTime,
      alarm :: ShortTime,
      dataA :: DataA,
      almostAwakes :: [LongTime]
}

data TimeDiff = TimeDiff { tdSeconds :: Int }

data DataA = DataA { daSeconds :: Int }

data Window = Window { minutes :: Int }

instance Show Date where
    show (Date day month year) = printf "%02d.%02d.%04d" day month year

instance Show LongTime where
    show (LongTime h m s) = printf "%02d:%02d:%02d" h m s

instance Show ShortTime where
    show (ShortTime hour minute) = printf "%02d:%02d" hour minute

instance Show TimeDiff where
    show (TimeDiff seconds) = printf "%2d" (seconds `div` 60)

instance Show DataA where
    show (DataA seconds) = printf "%02d:%02d" (seconds `div` 60) (seconds `mod` 60)

instance Show Window where
    show (Window minutes) = show minutes

instance Show Sleep where
    show s = "\n\
             \Date:                  " ++ show (date s) ++ "\n\
             \To Bed:                " ++ show (toBed s) ++ "\n\
             \Alarm Time:            " ++ show (alarm s) ++ "\n\
             \Effective Alarm Time:  " ++ (show $ last $ almostAwakes s) ++ "\n\
             \Window:                " ++ show (window s) ++ " min\n\
             \Data A (Clock):        " ++ show (dataA s) ++ " min\n\
             \Data A (Calculated):   " ++ show (computeDataA s) ++ " min\n\
             \\n\
             \Awake moments (" ++ show (length (almostAwakes s)) ++ "):\n" ++
             showAlmostAwakes (toBed s) (almostAwakes s) ++ "\n"

type Parser a = GenParser Int () a

sleepParser :: Int -> Parser Sleep
sleepParser year = do parseInt
                      date <- parseDate year
                      parseInt
                      window <- parseWindow
                      toBed <- parseShortTime
                      alarm <- parseShortTime
                      n <- parseInt
                      almostAwakes <- count n parseLongTime
                      dataA <- parseDataA
                      return $ Sleep date window toBed alarm dataA almostAwakes
    where
      parseDate :: Int -> Parser Date
      parseDate year = (\(m:d:_) -> Date d m year) <$> count 2 parseInt

      parseDataA :: Parser DataA
      parseDataA = DataA . sum . zipWith (*) [1,0xff] <$> count 2 parseInt

      parseWindow :: Parser Window
      parseWindow = Window <$> parseInt

      parseShortTime :: Parser ShortTime
      parseShortTime = (\(h:m:_) -> ShortTime h m) <$> count 2 parseInt

      parseLongTime :: Parser LongTime
      parseLongTime = (\(h:m:s:_) -> LongTime h m s) <$> count 3 parseInt

      parseInt :: Parser Int
      parseInt = tokenPrim show (\p _ _ -> p) Just

run = parseTest :: Parser Sleep -> [Int] -> IO ()

--

computeDataA :: Sleep -> DataA
computeDataA sleep = 
    DataA $ (`div` count) $ foldl1 diffSeconds $ map seconds [expand (toBed sleep),awake]
        where expand (ShortTime h m) = LongTime h m 0
              count = length (almostAwakes sleep)
              awake = last (almostAwakes sleep)

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
    where format n t d = printf " Data %2d: %s (slept:  %s min)" (n :: Int) (show t) (show d)
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
diffs f lst = zipWith f lst (tail lst)

browser :: Sleep -> String
browser s = printf "%s \"%s?%s\" &" browser uploadURL urlParam
    where browser = "firefox"
          uploadURL = "http://www.sleeptracker.net/import.php"
          urlParam :: String
          urlParam = printf "a=%s&w=%s&t=%s&dt=%s&da=%s"
                     (show $ alarm s)
                     (show $ window s)
                     (show $ toBed s)
                     (format $ almostAwakes s)
                     (show $ dataA s)
          format :: [LongTime] -> String
          format = concat . intersperse "," . map (show . shorten)
          shorten (LongTime h m s) = ShortTime h m

csv :: Sleep -> String
csv s = printf "%s;%s;%s;%s;%s;%s;%s;%s" 
        (show $ date s)
        (show $ toBed s)
        (show $ alarm s)
        (show $ last $ almostAwakes s)
        (show $ window s)
        (show $ dataA s)
        (show $ computeDataA s)
        (show $ length $ almostAwakes s)
        ++
        (concat $ map (printf ";%s" . show) $ almostAwakes s)
        ++ ";\n"

xml :: Sleep -> String
xml s = printf "<sleepRecord date=\"%s\">\n\
               \   <toBed>%s</toBed>\n\
               \   <alarmTime>%s</alarmTime>\n\
               \    <effectiveAlarmTime>%s</effectiveAlarmTime>\n\
               \   <window>%s</window>\n\
               \   <dataA clc=\"%s\">%s</dataA>\n\
               \   <awakeMoments cnt=\"%s\">\n\
               \   %s\
               \   </awakeMoments>\n\
               \<sleepRecord>\n"
               (show $ date s)
               (show $ toBed s)
               (show $ alarm s)
               (show $ last $ almostAwakes s)
               (show $ window s)
               (show $ dataA s)
               (show $ computeDataA s)
               (show $ length $ almostAwakes s)
               (concat $ zipWith format [1..] (almostAwakes s))
    where format :: Int -> LongTime -> String
          format n a = printf "<data count=\"%d\">%s</data>\n" n (show a)