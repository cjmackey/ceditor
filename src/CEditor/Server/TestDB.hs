{-# LANGUAGE TemplateHaskell #-}
module CEditor.Server.TestDB(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit()
import Database.HDBC

import CEditor.Server.DB

{-# ANN module "HLint: ignore Use camelCase" #-}

testDbInit :: IO ()
testDbInit = do
  conn <- initDB Nothing
  stmt <- prepare conn "create table if not exists objects (id varchar(22))"
  executeRaw stmt
  commit conn

case_init = testDbInit



testGroup = $(testGroupGenerator)
