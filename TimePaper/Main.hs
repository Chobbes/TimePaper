{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}

{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Arrow hiding ((|||))
import Data.Function
import Data.List
import Data.List.Split
import Data.Time.Format
import Data.Time.LocalTime
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import System.Environment
import System.Locale


-- | Entry for time and actions.
data TimeEntry = TimeEntry { time :: ZonedTime
                           , actions :: [String]
                           }
                           deriving (Show)

main :: IO ()
main = mainWith timePaper

timePaper :: FilePath -> IO (Diagram B R2)
timePaper logFile = do timeLog <- readFile logFile
                       return $ pad 1.5 $ timeTower 1.0 ((actionAmounts . parseTimeLog) timeLog)

-- | Create a tower with time percentages
timeTower :: Double -> [(String, Double)] -> Diagram B R2
timeTower length entries = vcat tower
             where sorted = sortBy (compare `on` snd) entries
                   (names, percents) = unzip sorted
                   blockLabel n h = alignedText 0 0.5 (n ++ " -- " ++ take 4 (show h) ++ "%")
                   towerBlock c n h = rect h 2 # fc c ||| strut 0.5 ||| blockLabel n h
                   towerColours = (concat . repeat) [red, yellow, green, blue, indigo, violet]
                   tower = zipWith3 towerBlock towerColours names percents

-- | Parse a bunch of time log lines!
parseTimeLog :: String -> [TimeEntry]
parseTimeLog = map parseEntry . lines

-- | Parse a single line from the time log.
parseEntry :: String -> TimeEntry
parseEntry line = TimeEntry (readTime defaultTimeLocale "%s" time) actions
  where (time:actions) = (filter (/= "") . splitOn " " . takeWhile (/= '[')) line

-- | Get a list of actions with the probability of the action
-- | throughout the day.
actionAmounts :: [TimeEntry] -> [(String, Double)]
actionAmounts entries = (map (head &&& percentage) . group . sort) acts
  where totalActions = length acts
        acts = concatMap actions entries
        percentage n = 100 * (fromIntegral . length) n / fromIntegral totalActions
