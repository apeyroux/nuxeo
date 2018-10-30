{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.Log (
  NuxeoLog
  , NuxeoLogType (..)
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

data NuxeoLogType = Debug | Error | Warning | Info deriving (Show, Read, Eq)

data NuxeoLogEntry = NuxeoLogEntry {
  nuxeoLogEntryDthr :: LocalTime
  , nuxeoLogEntryType :: NuxeoLogType
  , nuxeoLogEntrySection :: T.Text
  , nuxeoLogEntryAction :: T.Text
  , nuxeoLogEntryLog :: T.Text
  } deriving Show

type NuxeoLog = [NuxeoLogEntry]

-- | Parse Nuxeo server.log
--
-- @
-- parseNuxeoLog "./server.log"
--   >>= return . filter (\l -> nuxeoLogEntryType l == Error)
--   >>= mapM_ (\t ->
--                 putStrLn $ (show $ nuxeoLogEntryType t)
--                 <>  " "
--                 <> (Text.unpack $ nuxeoLogEntryAction t)
--                 <> " "
--                 <> (Text.unpack $ nuxeoLogEntryLog t)
--             )
-- @
parseNuxeoLog :: FilePath -> IO NuxeoLog
parseNuxeoLog logpath = runConduitRes $ sourceFile logpath .| sinkParser nuxeoLogParser

nuxeoLogParser :: Parser NuxeoLog
nuxeoLogParser = many $ (nuxeoLogEntryParser <* endOfLine)

nuxeoLogEntryParser :: Parser NuxeoLogEntry
nuxeoLogEntryParser = do
  ptime <- timeParser
  ptype <- space *> nuxeoTypeLogParser
  psection <- space *> nuxeoLogEntrySectionParser
  paction <- space *> nuxeoLogEntryActionParser
  pnuxeolog <- space *> manyTill' anyChar (try $ lookAhead $ (char '\n'
                                                               *> timeParser
                                                               *> space
                                                               *> nuxeoTypeLogParser
                                                               *> space
                                                               *> nuxeoLogEntryActionParser))
  return $ NuxeoLogEntry ptime ptype psection paction (T.pack pnuxeolog)

nuxeoTypeLogParser :: Parser NuxeoLogType
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
  _  <- char ',' *> count 3 digit
  return $
    LocalTime { localDay = fromGregorian (read y) (read mm) (read d)
              , localTimeOfDay = TimeOfDay (read h) (read m) (read s)
                }
