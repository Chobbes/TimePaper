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

import Control.Arrow
import Data.List
import Data.List.Split
import Data.Time.Format
import Data.Time.LocalTime
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Diagrams
import System.Environment
import System.Locale

-- | Entry for time and actions.
data TimeEntry = TimeEntry { time :: ZonedTime
                           , actions :: [String]
                           }
                           deriving (Show)

main = do [logFile, outputFile] <- getArgs
          timeLog <- readFile logFile
          makeChart outputFile (actionAmounts $ parseTimeLog timeLog)

-- | Simple pie chart stuff stolen from charts documentation.
makeChart output values = toFile def output $ do
  pie_title .= "Time Spent"
  pie_plot . pie_data .= map pitem values

-- | More chart stuff taken from: https://github.com/timbod7/haskell-chart/wiki/example%205
pitem (s,v) = pitem_value .~ v
            $ pitem_label .~ s
            $ def

-- | Parse a bunch of time log lines!
parseTimeLog :: String -> [TimeEntry]
parseTimeLog = map parseEntry . lines

-- | Parse a single line from the time log.
parseEntry :: String -> TimeEntry
parseEntry line = TimeEntry (readTime defaultTimeLocale "%s" time) actions
  where (time:actions) = (filter (/= "") . splitOn " " . takeWhile (/= '[')) line
  
-- | Get a list of actions, and the amount of times those actions
-- | appear within the logs.
actionAmounts :: [TimeEntry] -> [(String, Double)]
actionAmounts entries = (map (head &&& fromIntegral . length) . group . sort) acts
  where totalActions = length acts
        acts = concatMap actions entries
