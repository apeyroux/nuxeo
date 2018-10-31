{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Nuxeo.ElasticSearch
import           Nuxeo.Log
import           Nuxeo.Types
import           Options.Applicative


data NuxeoOpts = NuxeoOpts {
  instanceLogin :: T.Text
  , instancePassword :: T.Text
  , reindexUrl :: T.Text
  , showErrLogFile :: T.Text } deriving Show


parseOpts :: Parser NuxeoOpts
parseOpts = helper <*> do
  reindexUrl <- parseReindexUrl
  showErrLogFile <- parseShowErrLogFile
  instanceLogin <- parseInstanceLogin
  instancePassword <- parseInstancePassword
  return (NuxeoOpts {..})
  where
    parseReindexUrl = strOption (long "reindex" <> help "URL of the instance to be reindexed" <> value "")
    parseShowErrLogFile = strOption (long "log" <> help "Log file" <> value "")
    parseInstanceLogin = strOption (long "login" <> short 'u' <> help "Instance login"  <> value "")
    parseInstancePassword = strOption (long "password" <> short 'p' <> help "Instance password"  <> value "")


parserInfo :: ParserInfo NuxeoOpts
parserInfo = info parseOpts
  (progDesc "Nuxeo CLI"
   <>  header "nuxeo - nuxeo cli tools")

-- | Exemple reindex instance
reindexInstance :: Instance -> IO ()
reindexInstance i = do
  reindexAction <- reindex i
  case reindexAction of
    Left r -> TIO.putStrLn r
    Right r -> TIO.putStrLn r


-- | Exemple to extract errors from Nuxeo log
showErr :: T.Text -> IO()
showErr logFile = (filter (\l -> nuxeoLogEntryType l == Error) <$> parseNuxeoLog (T.unpack logFile))
                                >>= mapM_ (\t -> TIO.putStrLn $
                                                 T.replicate 10 "=" <> "\n"
                                                 <> T.replicate 5 " " <> (T.pack $ show $ nuxeoLogEntryDthr t)
                                                 <> "\n" <> T.replicate 10 "=" <> "\n"
                                                 <> (T.pack $ show $ nuxeoLogEntryType t)
                                                 <> " - "
                                                 <> nuxeoLogEntryAction t
                                                 <> "\n\n"
                                                 <> nuxeoLogEntryLog t
                                          )


main :: IO ()
main = do

  NuxeoOpts {..} <- execParser parserInfo

  if (instanceLogin /= ""
      && instancePassword /= ""
      && reindexUrl /= "") then reindexInstance (Instance reindexUrl instanceLogin instancePassword) else pure ()
  if showErrLogFile /= "" then showErr showErrLogFile else pure ()
