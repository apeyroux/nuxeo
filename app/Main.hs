{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Nuxeo.ElasticSearch
import           Nuxeo.Log
import           Options.Applicative


data NuxeoOpts = NuxeoOpts {
  instanceLogin :: String
  , instancePassword :: String
  , reindexUrl :: String
  , showErrLogFile :: String } deriving Show


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


reindexInstance :: String -> String -> String -> IO ()
reindexInstance urli login password = do
  reindexAction <- reindex login password urli
  case reindexAction of
    Left r -> putStrLn r
    Right r -> putStrLn r


-- | Exemple to extract errors from Nuxeo log
showErr :: String -> IO()
showErr logFile = (filter (\l -> nuxeoLogEntryType l == Error) <$> parseNuxeoLog logFile)
                                >>= mapM_ (\t -> TIO.putStrLn $
                                                 Text.replicate 10 "=" <> "\n"
                                                 <> Text.replicate 5 " " <> (Text.pack $ show $ nuxeoLogEntryDthr t)
                                                 <> "\n" <> Text.replicate 10 "=" <> "\n"
                                                 <> (Text.pack $ show $ nuxeoLogEntryType t)
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
      && reindexUrl /= "") then reindexInstance reindexUrl instanceLogin instancePassword else pure ()
  if showErrLogFile /= "" then showErr showErrLogFile else pure ()
