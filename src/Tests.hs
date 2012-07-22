{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.TH
import Test.Framework.Providers.HUnit()
import Test.Framework.Providers.QuickCheck2()

import CEditor.Server.TestDB
import CEditor.Common.TestOTObj
import CEditor.Common.TestOTClass

{-# ANN module "HLint: ignore Use camelCase" #-}


main :: IO ()
main = defaultMain [ $(testGroupGenerator)
                   , CEditor.Server.TestDB.testGroup
                   , CEditor.Common.TestOTObj.testGroup
                   , CEditor.Common.TestOTClass.testGroup
                   ]
