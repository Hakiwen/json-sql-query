{-# LANGUAGE OverloadedStrings #-}

module JsonParser where

import Types
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Data.Scientific (Scientific)

parseJsonFile :: FilePath -> IO (Either String [JsonRow])
parseJsonFile path = do
  content <- BL.readFile path
  case eitherDecode content of
    Left err -> return $ Left err
    Right val -> return $ parseJsonArray val

parseJsonArray :: Value -> Either String [JsonRow]
parseJsonArray (Array arr) = mapM parseJsonObject (V.toList arr)
parseJsonArray _ = Left "Expected JSON array at top level"

parseJsonObject :: Value -> Either String JsonRow
parseJsonObject (Object obj) = 
  M.traverseWithKey parseJsonValue (M.mapKeys (\k -> k) obj)
parseJsonObject _ = Left "Expected JSON object"

parseJsonValue :: Text -> Value -> Either String JsonValue
parseJsonValue _ (String t) = Right $ JsonString t
parseJsonValue _ (Number n) = Right $ JsonNumber n
parseJsonValue k _ = Left $ "Unsupported value type for key: " ++ T.unpack k