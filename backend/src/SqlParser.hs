{-# LANGUAGE OverloadedStrings #-}

module SqlParser where

import Types
import Data.Attoparsec.Text hiding (takeWhile)
import qualified Data.Attoparsec.Text as A
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (isAlphaNum)
import Data.Scientific (Scientific)

parseSql :: Text -> Either String SqlQuery
parseSql input = parseOnly sqlQuery input

sqlQuery :: Parser SqlQuery
sqlQuery = do
  skipSpace
  _ <- string "SELECT"
  skipSpace
  selectCl <- selectClause
  skipSpace
  _ <- string "FROM"
  skipSpace
  _ <- identifier
  skipSpace
  whereCl <- optional whereClause'
  skipSpace
  limitCl <- optional limitClause'
  skipSpace
  endOfInput
  return $ SqlQuery selectCl whereCl limitCl

selectClause :: Parser SelectClause
selectClause = selectAll <|> selectColumns
  where
    selectAll = char '*' >> return SelectAll
    selectColumns = SelectColumns <$> sepBy1 identifier (skipSpace >> char ',' >> skipSpace)

whereClause' :: Parser WhereClause
whereClause' = do
  _ <- string "WHERE"
  skipSpace
  whereExpr

whereExpr :: Parser WhereClause
whereExpr = orExpr

orExpr :: Parser WhereClause
orExpr = do
  left <- andExpr
  rest <- many $ do
    skipSpace
    _ <- string "OR"
    skipSpace
    andExpr
  return $ foldl Or left rest

andExpr :: Parser WhereClause
andExpr = do
  left <- primaryExpr
  rest <- many $ do
    skipSpace
    _ <- string "AND"
    skipSpace
    primaryExpr
  return $ foldl And left rest

primaryExpr :: Parser WhereClause
primaryExpr = parenExpr <|> conditionExpr
  where
    parenExpr = do
      _ <- char '('
      skipSpace
      expr <- whereExpr
      skipSpace
      _ <- char ')'
      return $ Parens expr
    
    conditionExpr = Condition <$> comparison

comparison :: Parser Comparison
comparison = do
  left <- value
  skipSpace
  op <- compOp'
  skipSpace
  right <- value
  return $ Comparison left op right

compOp' :: Parser CompOp
compOp' = (string "!=" >> return Neq)
     <|> (string "=" >> return Eq)
     <|> (string "<" >> return Lt)
     <|> (string ">" >> return Gt)

value :: Parser Value
value = numberLit <|> stringLit <|> colRef
  where
    numberLit = NumberLit <$> scientific
    stringLit = do
      _ <- char '\''
      content <- takeTill (== '\'')
      _ <- char '\''
      return $ StringLit content
    colRef = ColRef <$> identifier

identifier :: Parser Text
identifier = do
  first <- satisfy isAlpha
  rest <- A.takeWhile (\c -> isAlphaNum c || c == '_')
  return $ T.cons first rest
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

limitClause' :: Parser Int
limitClause' = do
  _ <- string "LIMIT"
  skipSpace
  decimal