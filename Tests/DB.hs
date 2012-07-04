{-# LANGUAGE TemplateHaskell #-}
module Tests.DB where

import CEditor.Server.DB
import Database.HDBC


testDbInit :: IO ()
testDbInit = do
  conn <- initDB Nothing
  stmt <- prepare conn "create table if not exists objects (id varchar(22))"
  executeRaw stmt
  commit conn


