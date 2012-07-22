{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses #-}
module CEditor.Common.TestOTClass(testGroup) where


import Test.Framework.TH
import Test.HUnit
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.DeriveTH

import Prelude hiding (concat)
import Debug.Trace ()
import Data.Maybe
import Data.Foldable
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as S
import Data.Map (Map)
import qualified Data.Map as M
import System.IO.Unsafe

import CEditor.Common.OTClass

{-# ANN module "HLint: ignore Use camelCase" #-}


insertText :: Int -> String -> String -> String
insertText ix s2 s1 = take ix s1 ++ s2 ++ drop ix s1

case_instex1 = "asdfblah" @=? insertText 0 "asdf" "blah"
case_instex2 = "asdfblah" @=? insertText (-1000) "asdf" "blah"
case_instex3 = "blahasdf" @=? insertText 1000 "asdf" "blah"
case_instex4 = "asdf" @=? insertText 1000 "asdf" ""
case_instex5 = "asdf" @=? insertText (-1000) "asdf" ""

data TextObj = TextObj Version String
             deriving (Show)

data TranBoilerplate = TranBoilerplate Version UserID
                     deriving (Show)

data TextTran = TextTran TranBoilerplate TextAct
              deriving (Show)
data TextAct = InsertText Int String
             deriving (Show)

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










$(derive makeArbitrary ''TextObj)
$(derive makeArbitrary ''TextAct)

data TextTestOp = C TextAct
                 | D | U
                 deriving (Show)
$(derive makeArbitrary ''TextTestOp)

data SimCliState = SimCliState { upq :: Seq TextTran
                               , downq :: Seq TextTran
                               , cli :: ClientObjectContainer TextObj TextTran }
                 deriving (Show)
data SimState = SimState { serv :: ObjectContainer TextObj TextTran 
                         , clis :: Map Int SimCliState }
              deriving (Show)

applyOp :: SimState -> (Int, TextTestOp) -> SimState
applyOp state (ix0, C a) = let ix = ix0 `mod` M.size (clis state)
                               cs = fromJust $ M.lookup ix $ clis state
                               t = TextTran (TranBoilerplate 0 (show ix)) a
                               (t', c') = applyClientTransformation t (cli cs)
                               cs' = cs { cli = c', upq = upq cs |> t' }
                           in state { clis = M.insert ix cs' (clis state) }
applyOp state (ix0, D) = let ix = ix0 `mod` M.size (clis state)
                             cs = fromJust $ M.lookup ix $ clis state
                             c = cli cs
                             c' = recvServerTransformation (S.index (downq cs) 0) c
                             cs' = cs { cli = c', downq = S.drop 1 (downq cs) }
                         in if S.length (downq cs) >= 1
                            then state { clis = M.insert ix cs' (clis state) }
                            else state
applyOp state (ix0, U) = let ix = ix0 `mod` M.size (clis state)
                             cs = fromJust $ M.lookup ix $ clis state
                             (t', s') = appendTransformation (S.index (upq cs) 0) (serv state)
                             cs' = cs { upq = S.drop 1 (upq cs) }
                             state' = state { clis = M.insert ix cs' (clis state), serv = s' }
                             state'' = state' { clis = M.map (endownqueue t') (clis state') }
                             endownqueue x ecs = ecs { downq = downq ecs |> x }
                         in if S.length (upq cs) >= 1
                            then state''
                            else state
flushDown :: SimCliState -> SimCliState
flushDown cs0 = cs0 { cli = Data.Foldable.foldl (flip recvServerTransformation)
                            (cli cs0) (downq cs0) }

case_sim = do
  let obj0 = TextObj 0 ""
  let cli0 i = SimCliState { upq = S.empty
                           , downq = S.empty
                           , cli = newClientObjectContainer (show i) obj0 }
  let simstate0 = SimState { clis = M.fromList [(ix, cli0 ix) | ix <- [0,1]]
                           , serv = newObjectContainer obj0 }
  "0" @=? userID (cli (fromJust $ M.lookup 0 $ clis simstate0))
  "1" @=? userID (cli (fromJust $ M.lookup 1 $ clis simstate0))
  let s1 = applyOp simstate0 (0, C (InsertText 0 "a"))
  1 @=? S.length (upq (fromJust $ M.lookup 0 $ clis s1))
  let s2 = applyOp s1 (0, U)
  0 @=? S.length (upq (fromJust $ M.lookup 0 $ clis s2))
  "a" @=? txt (current $ serv s2)
  1 @=? S.length (downq (fromJust $ M.lookup 0 $ clis s2))
  1 @=? S.length (downq (fromJust $ M.lookup 1 $ clis s2))
  let s3 = applyOp s2 (0, D)
  let s4 = applyOp s3 (1, D)
  "a" @=? txt (currentView ( cli (fromJust $ M.lookup 0 (clis s4))))
  "a" @=? txt (currentView ( cli (fromJust $ M.lookup 1 (clis s4))))  

prop_sim :: [(Int, TextTestOp)] -> [(Int, TextTestOp)] -> [(Int, TextTestOp)] -> [(Int, TextTestOp)] -> Bool
prop_sim op1 op2 op3 op4 = if res
               then --res
                 unsafePerformIO $ do
                   let tx = txt (currentView ( cli (fromJust $ M.lookup 0 finalClients)))
                   if Prelude.and [tx == "", length (
                                      filter (\(_,x) -> 
                                               case x of 
                                                 C (InsertText _ s) -> length s > 0 
                                                 _ -> False) ops
                                      ) > 0]
                     then print ops >> return False
                     else return True
               else unsafePerformIO $ do
                 print "ops"
                 print ops
                 print "ops''"
                 print ops''
                 print "simstate1"
                 print simstate1
                 print "finalClients"
                 print finalClients
                 print "out0"
                 print $ txt (currentView ( cli (fromJust $ M.lookup 0 finalClients)))
                 print "out1"
                 print $ txt (currentView ( cli (fromJust $ M.lookup 1 finalClients)))
                 print "done"
                 return False
  where ops = op1 ++ op2 ++ op3 ++ op4
        obj0 = TextObj 0 ""
        cli0 i = SimCliState { upq = S.empty
                             , downq = S.empty
                             , cli = newClientObjectContainer (show i) obj0 }
        simstate0 = SimState { clis = M.fromList [(ix, cli0 ix) | ix <- [0,1]]
                             , serv = newObjectContainer obj0 }
        ops'' = ops' ++ concat (replicate (length ops) [(ix, U) | ix <- [0,1]])
        --ops'' = ops' ++ [(ix, D) | _ <- replicate (length ops) 0, ix <- [0,1]]
        ops' = map (\(a, b) -> (a `mod` 2, b)) ops
        simstate1 = Data.Foldable.foldl applyOp simstate0 ops''
        finalClients = fmap flushDown (clis simstate1)
        res = txt (currentView ( cli (fromJust $ M.lookup 0 finalClients))) == 
              txt (currentView ( cli (fromJust $ M.lookup 1 finalClients)))





testGroup = $(testGroupGenerator)
