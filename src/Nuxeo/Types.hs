{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.Types (
  NuxeoInstance (..)
  , NuxeoLog
  , NuxeoLogEntry (..)
  , NuxeoLogType (..)
  ) where

import Data.Text
import Data.Time

data NuxeoLogType = NuxeoDebug | NuxeoError | NuxeoWarning | NuxeoInfo deriving (Show, Read, Eq)

data NuxeoLogEntry = NuxeoLogEntry {
  nuxeoLogEntryDthr :: LocalTime
  , nuxeoLogEntryType :: NuxeoLogType
  , nuxeoLogEntrySection :: Text
  , nuxeoLogEntryAction :: Text
  , nuxeoLogEntryLog :: Text
  } deriving Show

type NuxeoLog = [NuxeoLogEntry]

data NuxeoInstance = NuxeoInstance {
  instanceUrl :: Text
  , instanceLogin :: Text
  , instancePassword :: Text
  } deriving (Read, Show)
