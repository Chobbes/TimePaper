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

{-# LANGUAGE OverloadedStrings #-}

module TimePaper.Parser (parseTimeLog) where

import TimePaper.TimeEntry
import TimePaper.WeekDay

import Control.Applicative
import Data.Attoparsec.Text hiding (take, takeWhile)
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime


-- | Parse a bunch of time log lines!
parseTimeLog :: Parser [TimeEntry]
parseTimeLog = many parseEntry

-- | Parse a single line from the time log.
parseEntry :: Parser TimeEntry
parseEntry = do epochTime <- decimal
                skipSpace
                actions <- many parseAction
                (date, time, weekDay) <- parseTimeStamp
                endOfLine
                return (TimeEntry date time weekDay actions)

-- | Parsing of an action. These are just strings separated by spaces.
parseAction :: Parser String
parseAction = do action <- many (letter <|> digit)
                 char ' '
                 skipSpace
                 return action
                 
-- | Parse the timestamp for an entry in the log.
parseTimeStamp :: Parser (Day, TimeOfDay, WeekDay)
parseTimeStamp = do char '['
                    date <- parseDate
                    skipSpace
                    time <- parseTime
                    skipSpace
                    weekDay <- parseWeekDay
                    char ']'
                    return (date, time, weekDay)

-- | Parse date strings with a format like: 2014.09.20
parseDate :: Parser Day
parseDate = do year <- decimal
               char '.'
               month <- decimal
               char '.'
               day <- decimal
               return (fromGregorian year month day)

-- | Parse the time of day.
parseTime :: Parser TimeOfDay
parseTime = do hours <- decimal
               char ':'
               minutes <- decimal
               char ':'
               seconds <- decimal
               return (TimeOfDay hours minutes (fromIntegral seconds))

-- | Parse the day of the week field.
parseWeekDay :: Parser WeekDay
parseWeekDay = do string "Mon"; return Monday
           <|> do string "Tue"; return Tuesday
           <|> do string "Wed"; return Wednesday
           <|> do string "Thu"; return Thursday
           <|> do string "Fri"; return Friday
           <|> do string "Sat"; return Saturday
           <|> do string "Sun"; return Sunday

