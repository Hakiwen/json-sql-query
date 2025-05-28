{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import qualified Data.Map as M
import Data.Scientific (fromFloatDigits)

import Types
import SqlParser
import QueryExecutor
import JsonParser

main :: IO ()
main = hspec $ do
  describe "SQL Parser" $ do
    it "parses SELECT * FROM table" $ do
      let result = parseSql "SELECT * FROM table"
      result `shouldBe` Right (SqlQuery SelectAll Nothing Nothing)
    
    it "parses SELECT with specific columns" $ do
      let result = parseSql "SELECT state, pop FROM table"
      result `shouldBe` Right (SqlQuery (SelectColumns ["state", "pop"]) Nothing Nothing)
    
    it "parses SELECT with LIMIT" $ do
      let result = parseSql "SELECT * FROM table LIMIT 5"
      result `shouldBe` Right (SqlQuery SelectAll Nothing (Just 5))
    
    it "parses WHERE with number comparison" $ do
      let result = parseSql "SELECT * FROM table WHERE pop > 1000000"
      case result of
        Right (SqlQuery SelectAll (Just (Condition (Comparison (ColRef "pop") Gt (NumberLit _)))) Nothing) -> True
        _ -> False
      `shouldBe` True
    
    it "parses WHERE with string comparison" $ do
      let result = parseSql "SELECT * FROM table WHERE state = 'California'"
      case result of
        Right (SqlQuery SelectAll (Just (Condition (Comparison (ColRef "state") Eq (StringLit "California")))) Nothing) -> True
        _ -> False
      `shouldBe` True
    
    it "parses complex WHERE with AND/OR" $ do
      let result = parseSql "SELECT * FROM table WHERE (pop > 1000 AND state = 'CA') OR region = 'West'"
      case result of
        Right (SqlQuery SelectAll (Just _) Nothing) -> True
        _ -> False
      `shouldBe` True
    
    it "fails on invalid SQL" $ do
      let result = parseSql "SELECT FROM table"
      case result of
        Left _ -> True
        _ -> False
      `shouldBe` True
  
  describe "Query Executor" $ do
    let testData = [
          M.fromList [("state", JsonString "California"), ("pop", JsonNumber 39538223), ("region", JsonString "West")],
          M.fromList [("state", JsonString "Texas"), ("pop", JsonNumber 29145505), ("region", JsonString "South")],
          M.fromList [("state", JsonString "Florida"), ("pop", JsonNumber 21538187), ("region", JsonString "South")]
        ]
    
    it "executes SELECT * FROM table" $ do
      let query = SqlQuery SelectAll Nothing Nothing
      let result = executeQuery query testData
      case result of
        Right rows -> length rows `shouldBe` 3
        Left _ -> False `shouldBe` True
    
    it "executes SELECT with specific columns" $ do
      let query = SqlQuery (SelectColumns ["state", "pop"]) Nothing Nothing
      let result = executeQuery query testData
      case result of
        Right rows -> do
          length rows `shouldBe` 3
          M.size (head rows) `shouldBe` 2
        Left _ -> False `shouldBe` True
    
    it "executes WHERE with number comparison" $ do
      let query = SqlQuery SelectAll (Just (Condition (Comparison (ColRef "pop") Gt (NumberLit 30000000)))) Nothing
      let result = executeQuery query testData
      case result of
        Right rows -> length rows `shouldBe` 1
        Left _ -> False `shouldBe` True
    
    it "executes WHERE with string comparison" $ do
      let query = SqlQuery SelectAll (Just (Condition (Comparison (ColRef "state") Eq (StringLit "Texas")))) Nothing
      let result = executeQuery query testData
      case result of
        Right rows -> length rows `shouldBe` 1
        Left _ -> False `shouldBe` True
    
    it "executes with LIMIT" $ do
      let query = SqlQuery SelectAll Nothing (Just 2)
      let result = executeQuery query testData
      case result of
        Right rows -> length rows `shouldBe` 2
        Left _ -> False `shouldBe` True