{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module CEditor.Common.TestOTClass(testGroup) where


import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.DeriveTH

import CEditor.Common.OTClass

{-# ANN module "HLint: ignore Use camelCase" #-}


insertText :: Int -> String -> String -> String
insertText ix s2 s1 = take ix s1 ++ s2 ++ drop ix s1

data TextObj = TextObj Version String

data TranBoilerplate = TranBoilerplate Version UserID

data TextTran = TextTran TranBoilerplate TextAct
data TextAct = InsertText Int String

instance Versioned TextObj where
  version (TextObj v _) = v
  setVersion v (TextObj _ s) = TextObj v s
instance Versioned TranBoilerplate where
  version (TranBoilerplate v _) = v
  setVersion v (TranBoilerplate _ u) = TranBoilerplate v u
instance UserTagged TranBoilerplate where
  userID (TranBoilerplate _ u) = u
instance Versioned TextTran where
  version (TextTran b _) = version b
  setVersion v (TextTran b a) = TextTran (setVersion v b) a
instance UserTagged TextTran where
  userID (TextTran b _) = userID b

txt (TextObj _ s) = s

instance Transformation TextTran where
  transformTransformation (TextTran _ (InsertText ix0 s0)) (TextTran b (InsertText ix1 s1)) =
    TextTran b (if ix1 >= ix0
                then InsertText (ix1 + length s0) s1
                else InsertText ix1 s1)
instance OT TextObj TextTran where
  transformObject (TextTran _ (InsertText ix s)) (TextObj v0 s0) = TextObj (v0 + 1) (insertText ix s s0)


$(derive makeArbitrary ''TextObj)
$(derive makeArbitrary ''TextAct)

data TextTestOp = C0 TextTran
                | C1 TextTran
                | C2 TextTran



case_initialize = do
  let obj0 = TextObj 0 ""
  let s0 = newObjectContainer obj0 :: ObjectContainer TextObj TextTran
  let c0_0 = newClientObjectContainer "0" obj0 :: ClientObjectContainer TextObj TextTran
  let c1_0 = newClientObjectContainer "1" obj0 :: ClientObjectContainer TextObj TextTran
  txt (currentView c0_0) @=? ""
  txt (serverView c0_0) @=? ""
  txt (currentView c1_0) @=? ""
  txt (serverView c1_0) @=? ""
  -- c0 writes "asdf"
  let t0_0 = TextTran (TranBoilerplate 0 "0") (InsertText 0 "asdf")
  let (_, c0_1) = applyClientTransformation t0_0 c0_0
  "asdf" @=? txt (currentView c0_1)
  "" @=? txt (serverView c0_1)
  -- server receives "asdf" from c0
  let (_, s1) = appendTransformation t0_0 s0
  "asdf" @=? txt (current s1)
  -- c0 receives acknowledgement from server
  let c0_2 = recvServerTransformation t0_0 c0_1
  "asdf" @=? txt (currentView c0_2)
  "asdf" @=? txt (serverView c0_2)
  -- c1 writes "blah"
  let t1_1 = TextTran (TranBoilerplate 0 "1") (InsertText 0 "blah")
  let (_, c1_1) = applyClientTransformation t1_1 c1_0
  txt (currentView c1_1) @=? "blah"
  txt (serverView c1_1) @=? ""
  -- server receives "blah" from c1
  let (t1_1', s2) = appendTransformation t1_1 s1
  txt (current s2) @=? "asdfblah"
  -- c1 receives c0's "asdf" from server
  let c1_2 = recvServerTransformation t0_0 c1_1
  txt (currentView c1_2) @=? "asdfblah"
  txt (serverView c1_2) @=? "asdf"
  -- c1 receives acknowledgement from server (modified by server due to c0's "asdf")
  let c1_3 = recvServerTransformation t1_1' c1_2
  txt (currentView c1_3) @=? "asdfblah"
  txt (serverView c1_3) @=? "asdfblah"
  -- c0 receives c1's "blah" from server (modified by server due to c0's "asdf")
  let c0_3 = recvServerTransformation t1_1' c0_2
  txt (currentView c0_3) @=? "asdfblah"
  txt (serverView c0_3) @=? "asdfblah"




testGroup = $(testGroupGenerator)
