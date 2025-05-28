{-# LANGUAGE OverloadedStrings #-}

module JsonParser where

import Types
import Data.Aeson hiding (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (toText)
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

parseJsonArray :: A.Value -> Either String [JsonRow]
parseJsonArray (A.Array arr) = mapM parseJsonObject (V.toList arr)
parseJsonArray _ = Left "Expected JSON array at top level"

parseJsonObject :: A.Value -> Either String JsonRow
parseJsonObject (A.Object obj) = 
  M.traverseWithKey parseJsonValue (M.fromList $ map (\(k,v) -> (toText k, v)) $ KM.toList obj)
parseJsonObject _ = Left "Expected JSON object"

parseJsonValue :: Text -> A.Value -> Either String JsonValue
parseJsonValue _ (A.String t) = Right $ JsonString t
parseJsonValue _ (A.Number n) = Right $ JsonNumber n
parseJsonValue k _ = Left $ "Unsupported value type for key: " ++ T.unpack k