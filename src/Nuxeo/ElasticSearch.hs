{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.ElasticSearch (
  reindex
  ) where

import           Data.Aeson (object, (.=))
import qualified Data.ByteString.Char8 as C8
import           Network.HTTP.Simple
import           Network.URL

-- | POST
-- @
-- {"params":{},"context":{}}
-- @
-- to __"/nuxeo/site/automation/Elasticsearch.Index"__
reindex :: String -> String -> String -> IO (Either String String)
reindex
  instanceLogin
  instancePassword
  instanceUrl =
  -- check if instanceUrl is formated
  case importURL esUrl of
    Just _ -> do
      initReq <- parseRequest req
      let request = setRequestBasicAuth (C8.pack instanceLogin) (C8.pack instancePassword) $ setRequestBodyJSON requestObject initReq
      response <- httpLBS request
      if getResponseStatusCode response /= 204 
        then pure $ Left $ "Request error: " <> (show $ getResponseStatusCode response)
        else pure $ Right "Reindexing taken in account"
    Nothing -> pure $ Left "Request error: bad url"
  where
    esUrlPath = "/nuxeo/site/automation/Elasticsearch.Index"
    esUrl = instanceUrl <> esUrlPath
    req = "POST " <> esUrl
    requestObject = object ["params" .= object [], "context"  .= object []]
