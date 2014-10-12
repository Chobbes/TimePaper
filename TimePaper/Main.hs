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

import TimePaper.Parser
import TimePaper.TimeEntry

import Control.Arrow hiding ((|||))
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude
import System.Environment
import System.Locale
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.List
import Data.Function
import Data.Text.IO as T
import Data.Either


main :: IO ()
main = mainWith timePaper

-- | Reads a time log in order to generate a wallpaper.
timePaper :: FilePath -> IO (Diagram B R2)
timePaper logFile = do timeLog <- T.readFile logFile
                       return . pad 1.5 . timeTower 1.0 $ (actionAmounts . takeRight . parseOnly parseTimeLog) timeLog
                       where takeRight (Right a) = a
                             takeRight (Left a) = []

-- | Create a tower with time percentages
timeTower :: Double -> [(String, Double)] -> Diagram B R2
timeTower length entries = vcat tower
             where sorted = sortBy (compare `on` snd) entries
                   (names, percents) = unzip sorted
                   blockLabel n h = alignedText 0 0.5 (n ++ " -- " ++ take 4 (show h) ++ "%") # showOrigin
                   towerBlock c n h = rect h 2 # fc c ||| (strut 0.5 # showOrigin) ||| blockLabel n h
                   towerColours = (concat . repeat) [red, yellow, green, blue, indigo, violet]
                   tower = zipWith3 towerBlock towerColours names percents


-- | Get a list of actions with the probability of the action
-- | throughout the day.
actionAmounts :: [TimeEntry] -> [(String, Double)]
actionAmounts entries = (map (head &&& percentage) . group . sort) acts
  where totalActions = length acts
        acts = concatMap actions entries
        percentage n = 100 * (fromIntegral . length) n / fromIntegral totalActions
