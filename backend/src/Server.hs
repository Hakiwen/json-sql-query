{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Server where

import Types
import SqlParser
import QueryExecutor
import Database
import Web.Scotty.Trans
import Network.Wai.Middleware.Cors
import Control.Monad.Reader
import Control.Monad.IO.Class
import Database.SQLite.Simple hiding (query)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson (encode, decode)
import qualified Data.Map as M

data Env = Env
  { envData :: [JsonRow]
  , envConn :: Connection
  }

type AppM = ReaderT Env IO

runServer :: Int -> Env -> IO ()
runServer port env = scottyT port (`runReaderT` env) $ do
  middleware simpleCors
  
  post "/query" $ do
    req <- jsonData :: ActionT TL.Text AppM QueryRequest
    env' <- lift ask
    let queryText = query req
    
    case parseSql queryText of
      Left err -> do
        let errorMsg = "Parse error: " <> T.pack err
        liftIO $ saveQuery (envConn env') queryText errorMsg
        json $ QueryResponse $ Left errorMsg
      Right sqlQuery -> do
        case executeQuery sqlQuery (envData env') of
          Left err -> do
            liftIO $ saveQuery (envConn env') queryText err
            json $ QueryResponse $ Left err
          Right results -> do
            let textResults = map convertRowToTextMap results
            let resultText = T.pack $ show (length results) <> " rows returned"
            liftIO $ saveQuery (envConn env') queryText resultText
            json $ QueryResponse $ Right textResults
  
  get "/history" $ do
    env' <- lift ask
    history <- liftIO $ getHistoricalQueries (envConn env')
    let historyList = map (\(id, q, r, t) -> 
          HistoricalQuery id q r t) history
    json historyList