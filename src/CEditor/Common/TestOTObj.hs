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

$(derive makeArbitrary ''ObjData)
$(derive makeArbitrary ''ActionDesc)

case_create = do
  [Obj "asdf" 0 ErrorObj] @=? applyActionToObjs (CreateObjectAction "asdf" ErrorObj) []

case_insert = do
  [Obj "asdf" 5 (TextObj "blBLAHah")] @=? applyActionToObjs (InsertTextAction "asdf" 2 "BLAH") [Obj "asdf" 4 (TextObj "blah")]

prop_pinsert ix v s1 s2 =
  if ix >= 0
  then case act of
    [Obj "asdf" v2 (TextObj s)] -> and [ s == insertText ix s2 s1
                                       , v + 1 == v2 ]
    _ -> False
  else act == []
    where types = (ix :: Integer, v :: Integer, s1 :: String, s2 :: String)
          act = applyActionToObjs (InsertTextAction "asdf" ix s2) [Obj "asdf" v (TextObj s1)]

case_insertinsert = do
  let o0 =  Obj "asdf" 4 (TextObj "blah")
  let a1 = InsertTextAction "asdf" 2 "BLAH"
  let a2 = InsertTextAction "asdf" 2 "ASDF"
  let a3 = InsertTextAction "asdf" 3 "WARGLE"
  let o1 = head $ applyActionToObjs a1 [o0]
  let a2' = head $ applyActionToAction a1 a2
  let o2 = head $ applyActionToObjs a2' [o1]
  let a3' = head $ applyActionToAction a1 a3
  let a3'' = head $ applyActionToAction a2' a3'
  let o3 = head $ applyActionToObjs a3'' [o2]
  o0 @=? Obj "asdf" 4 (TextObj "blah")
  o1 @=? Obj "asdf" 5 (TextObj "blBLAHah")
  o2 @=? Obj "asdf" 6 (TextObj "blBLAHASDFah")
  o3 @=? Obj "asdf" 7 (TextObj "blBLAHASDFaWARGLEh")

prop_idenc x = x == (decode $ encode x)
  where types = x::ObjData

testGroup = $(testGroupGenerator)
