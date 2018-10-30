{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import           Nuxeo.Log
import           Options.Applicative

data NuxeoCli = NuxeoCli { cliLogFile :: String }

args :: Parser NuxeoCli
args = NuxeoCli
       <$> strOption (long "log" <> help "Log file")

-- | Exemple to extract errors from Nuxeo log
nuxeocli :: NuxeoCli -> IO()
nuxeocli (NuxeoCli logFile) = (filter (\l -> nuxeoLogEntryType l == Error) <$> parseNuxeoLog logFile)
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

main :: IO()
main = nuxeocli =<< execParser opts
  where
    opts = info (args <**> helper)
      (fullDesc
        <> progDesc "Nuxeo CLI"
        <> header "nuxeo - nuxeo cli tools"
      )
