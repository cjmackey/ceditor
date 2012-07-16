{-# LANGUAGE TemplateHaskell #-}
module CEditor.Common.TestOTObj(testGroup) where

import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Binary(encode, decode)
import Data.DeriveTH


import CEditor.Common.OTObj

$(derive makeArbitrary ''Obj)


prop_idenc x = x == (decode $ encode x)
  where types = x::Obj

testGroup = $(testGroupGenerator)
