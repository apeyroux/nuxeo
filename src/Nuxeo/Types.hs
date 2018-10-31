{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.Types (
  Instance (..)
  ) where

import Data.Text 

data Instance = Instance {
  instanceUrl :: Text
  , instanceLogin :: Text
  , instancePassword :: Text
  } deriving (Read, Show)
