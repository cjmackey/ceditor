{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.TH
import Test.Framework.Providers.HUnit()
import Test.Framework.Providers.QuickCheck2()

import CEditor.Server.DBTests

{-# ANN module "HLint: ignore Use camelCase" #-}


main :: IO ()
main = defaultMain [ $(testGroupGenerator)
                   , CEditor.Server.DBTests.testGroup
                   ]
