{-# LANGUAGE TemplateHaskell #-}


import CEditor.Server.DB
import Test.Framework.TH
import Test.Framework
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2


case_1 = do 
  1 @=? 1
case_2 = do
  2 @=? 2
prop_reverse xs = reverse (reverse xs) == xs
  where types = xs::[Int]

case_dbInit = do
  conn <- initDB Nothing
  return ()

main = $(defaultMainGenerator)




