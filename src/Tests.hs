{-# LANGUAGE TemplateHaskell #-}

module Main where

import Tests.DB
import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2

{-# ANN module "HLint: ignore Use camelCase" #-}

case_initDB :: IO ()
case_initDB = testDbInit

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

main :: IO ()
main = $(defaultMainGenerator)




