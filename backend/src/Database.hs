{-# LANGUAGE OverloadedStrings #-}

module Database where

import Database.SQLite.Simple
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

initDatabase :: Connection -> IO ()
initDatabase conn = execute_ conn 
  "CREATE TABLE IF NOT EXISTS queries (\
  \id INTEGER PRIMARY KEY AUTOINCREMENT, \
  \query_text TEXT NOT NULL, \
  \result TEXT NOT NULL, \
  \timestamp TEXT NOT NULL)"

saveQuery :: Connection -> Text -> Text -> IO ()
saveQuery conn queryText result = do
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  execute conn "INSERT INTO queries (query_text, result, timestamp) VALUES (?, ?, ?)"
    (queryText, result, timestamp)

getHistoricalQueries :: Connection -> IO [(Int, Text, Text, Text)]
getHistoricalQueries conn = 
  query_ conn "SELECT id, query_text, result, timestamp FROM queries ORDER BY id DESC"