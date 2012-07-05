{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework (defaultMain)
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

import Tests.DB
import Tests.TemplParser

{-# ANN module "HLint: ignore Use camelCase" #-}

case_initDB :: IO ()
case_initDB = testDbInit

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = defaultMain [ $(testGroupGenerator)
                   , Tests.TemplParser.testGroup
                   ]
