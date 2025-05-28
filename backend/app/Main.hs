{-# LANGUAGE OverloadedStrings #-}

module Main where

import Server
import Types
import JsonParser
import Database
import Database.SQLite.Simple
import System.Environment (getArgs)
import Control.Monad (when)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [jsonFile] -> runApp jsonFile "db.db"
    [jsonFile, dbFile] -> runApp jsonFile dbFile
    _ -> putStrLn "Usage: json-sql-query-exe <json-file> [db-file]"

runApp :: FilePath -> FilePath -> IO ()
runApp jsonFile dbFile = do
  result <- parseJsonFile jsonFile
  case result of
    Left err -> putStrLn $ "Failed to parse JSON file: " ++ err
    Right jsonData -> do
      conn <- open dbFile
      initDatabase conn
      let env = Env jsonData conn
      putStrLn $ "Server starting on port 3000..."
      putStrLn $ "JSON data loaded from: " ++ jsonFile
      putStrLn $ "Database: " ++ dbFile
      runServer 3000 env