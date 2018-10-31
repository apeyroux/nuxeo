{-# LANGUAGE OverloadedStrings #-}

module Nuxeo.ElasticSearch (
  reindex
  ) where

import Data.Aeson (object, (.=))
import Data.Text as Text
import Data.Text.Encoding
import Network.HTTP.Simple
import Network.URL
import Nuxeo.Types

-- | POST @{"params":{},"context":{}}@ to @/nuxeo/site/automation/Elasticsearch.Index@
reindex :: Instance -> IO (Either Text Text)
reindex i =
  -- check if instanceUrl is formated
  case importURL esUrl of
    Just _ -> do
      initReq <- parseRequest req
      let request = setRequestBasicAuth (encodeUtf8 $ instanceLogin i) (encodeUtf8 $ instancePassword i) $ setRequestBodyJSON requestObject initReq
      response <- httpLBS request
      if getResponseStatusCode response /= 204 
        then pure $ Left $ "Request error: " <> (Text.pack $ show $ getResponseStatusCode response)
        else pure $ Right "Reindexing taken in account"
    Nothing -> pure $ Left "Request error: bad url"
  where
    esUrlPath = "/nuxeo/site/automation/Elasticsearch.Index"
    esUrl = (Text.unpack $ instanceUrl i) <> esUrlPath
    req = "POST " <> esUrl
    requestObject = object ["params" .= object [], "context"  .= object []]
