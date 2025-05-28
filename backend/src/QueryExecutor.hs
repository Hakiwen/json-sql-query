{-# LANGUAGE OverloadedStrings #-}

module QueryExecutor where

import Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Scientific (Scientific, toBoundedInteger, toRealFloat)
import Data.Maybe (fromMaybe)

executeQuery :: SqlQuery -> [JsonRow] -> Either Text [JsonRow]
executeQuery (SqlQuery selectCl whereCl limitCl) rows =
  let filtered = case whereCl of
        Nothing -> rows
        Just wc -> filter (evaluateWhere wc) rows
      selected = map (selectRow selectCl) filtered
      limited = case limitCl of
        Nothing -> selected
        Just n -> take n selected
  in Right limited

selectRow :: SelectClause -> JsonRow -> JsonRow
selectRow SelectAll row = row
selectRow (SelectColumns cols) row = 
  M.fromList [(col, fromMaybe (JsonString "") (M.lookup col row)) | col <- cols]

evaluateWhere :: WhereClause -> JsonRow -> Bool
evaluateWhere (Condition comp) row = evaluateComparison comp row
evaluateWhere (And left right) row = evaluateWhere left row && evaluateWhere right row
evaluateWhere (Or left right) row = evaluateWhere left row || evaluateWhere right row
evaluateWhere (Parens expr) row = evaluateWhere expr row

evaluateComparison :: Comparison -> JsonRow -> Bool
evaluateComparison (Comparison left op right) row =
  case (getValue left row, getValue right row) of
    (Just v1, Just v2) -> compareValues op v1 v2
    _ -> False

getValue :: Value -> JsonRow -> Maybe JsonValue
getValue (ColRef col) row = M.lookup col row
getValue (StringLit s) _ = Just $ JsonString s
getValue (NumberLit n) _ = Just $ JsonNumber n

compareValues :: CompOp -> JsonValue -> JsonValue -> Bool
compareValues Eq (JsonString s1) (JsonString s2) = s1 == s2
compareValues Eq (JsonNumber n1) (JsonNumber n2) = n1 == n2
compareValues Neq v1 v2 = not (compareValues Eq v1 v2)
compareValues Lt (JsonNumber n1) (JsonNumber n2) = n1 < n2
compareValues Gt (JsonNumber n1) (JsonNumber n2) = n1 > n2
compareValues _ _ _ = False

jsonValueToText :: JsonValue -> Text
jsonValueToText (JsonString s) = s
jsonValueToText (JsonNumber n) = T.pack $ show n

convertRowToTextMap :: JsonRow -> M.Map Text Text
convertRowToTextMap = M.map jsonValueToText