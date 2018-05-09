{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.Log (
  NuxeoTypeLog (..)
  , NuxeoLogEntry (..)
  , parseNuxeoLog
  ) where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.Combinator
import           Data.Conduit
import           Data.Conduit.Attoparsec
import           Data.Conduit.Binary
import qualified Data.Text as T
import           Data.Time

type Verbose = Bool

data NuxeoTypeLog = Debug | Error | Warning | Info deriving (Show, Read, Eq)

data NuxeoLogEntry = NuxeoLogEntry {
  nuxeoLogEntryDthr :: LocalTime
  , nuxeoLogEntryType :: NuxeoTypeLog
  , nuxeoLogEntrySection :: T.Text
  , nuxeoLogEntryAction :: T.Text
  , nuxeoLogEntryLog :: T.Text
  } deriving Show

type NuxeoLog = [NuxeoLogEntry]

-- |Parse Nuxeo server.log
-- > logs <- parseNuxeoLog v "/opt/nuxeo-data/log/server.log"
-- > filter (\l -> (nuxeoLogEntryType l == Error)
-- >            || (nuxeoLogEntryType l == Warning)) logs
parseNuxeoLog :: Verbose -> FilePath -> IO NuxeoLog
parseNuxeoLog v logpath = runConduitRes $ sourceFile logpath .| sinkParser (nuxeoLogParser v)

nuxeoLogParser :: Verbose -> Parser NuxeoLog
nuxeoLogParser v = many $ (nuxeoLogEntryParser v <* endOfLine)

nuxeoLogEntryParser :: Verbose -> Parser NuxeoLogEntry
nuxeoLogEntryParser v = do
  letime <- timeParser
  letype <- space *> nuxeoTypeLogParser
  section <- space *> nuxeoLogEntrySectionParser
  action <- space *> nuxeoLogEntryActionParser
  nuxeolog <- case v of
    True -> space *> manyTill' anyChar (try $ lookAhead $ char '\n' *> timeParser)
    False -> space *> manyTill' anyChar (try $ lookAhead $ char '\n')
  return $ NuxeoLogEntry letime letype section action (T.pack nuxeolog)

nuxeoTypeLogParser :: Parser NuxeoTypeLog
nuxeoTypeLogParser = (string "ERROR" *> return Error)
                     <|> (string "DEBUG" *> return Debug)
                     <|> (string "WARN" *> return Warning)
                     <|> (string "INFO" *> return Info)
                     <|> fail "Invalid NuxeoTypeLog"

nuxeoLogEntrySectionParser :: Parser T.Text
nuxeoLogEntrySectionParser = char '[' *> manyTill' anyChar (char ']')
  <|> fail "Oops parse section" >>= return . T.pack

nuxeoLogEntryActionParser :: Parser T.Text
nuxeoLogEntryActionParser = char '[' *> manyTill' anyChar (char ']')
  <|> fail "Oops parse action" >>= return . T.pack

timeParser :: Parser LocalTime
timeParser = do
  y  <- count 4 digit
  mm <- char '-' *> count 2 digit
  d  <- char '-' *> count 2 digit
  h  <- char ' ' *> count 2 digit
  m  <- char ':' *> count 2 digit
  s  <- char ':' *> count 2 digit
        *> char ','
        *> count 3 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }