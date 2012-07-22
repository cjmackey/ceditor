{-# LANGUAGE TemplateHaskell #-}
module CEditor.Common.TestOTClass(testGroup) where


import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.Binary(encode, decode)
import Data.DeriveTH

import CEditor.Common.OTClass

{-# ANN module "HLint: ignore Use camelCase" #-}


testGroup = $(testGroupGenerator)



