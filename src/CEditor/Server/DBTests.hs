{-# LANGUAGE TemplateHaskell #-}
module CEditor.Server.DBTests(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit()
import CEditor.Server.DB
import Database.HDBC


testDbInit :: IO ()
testDbInit = do
  conn <- initDB Nothing
  stmt <- prepare conn "create table if not exists objects (id varchar(22))"
  executeRaw stmt
  commit conn

case_init = testDbInit



testGroup = $(testGroupGenerator)
