{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Text (Text)
import Data.Aeson
import GHC.Generics
import qualified Data.Map as M
import Data.Scientific (Scientific)

type JsonRow = M.Map Text JsonValue

data JsonValue 
  = JsonString Text
  | JsonNumber Scientific
  deriving (Show, Eq)

data SqlQuery = SqlQuery
  { selectColumns :: SelectClause
  , whereClause   :: Maybe WhereClause
  , limitClause   :: Maybe Int
  } deriving (Show, Eq)

data SelectClause 
  = SelectAll
  | SelectColumns [Text]
  deriving (Show, Eq)

data WhereClause 
  = Condition Comparison
  | And WhereClause WhereClause
  | Or WhereClause WhereClause
  | Parens WhereClause
  deriving (Show, Eq)

data Comparison = Comparison
  { compLeft  :: Value
  , compOp    :: CompOp
  , compRight :: Value
  } deriving (Show, Eq)

data CompOp = Eq | Neq | Lt | Gt
  deriving (Show, Eq)

data Value 
  = ColRef Text
  | StringLit Text
  | NumberLit Scientific
  deriving (Show, Eq)

data QueryRequest = QueryRequest
  { query :: Text
  } deriving (Generic)

instance FromJSON QueryRequest
instance ToJSON QueryRequest

data QueryResponse = QueryResponse
  { results :: Either Text [M.Map Text Text]
  } deriving (Generic)

instance ToJSON QueryResponse

data HistoricalQuery = HistoricalQuery
  { id :: Int
  , queryText :: Text
  , result :: Text
  , timestamp :: Text
  } deriving (Generic)

instance ToJSON HistoricalQuery