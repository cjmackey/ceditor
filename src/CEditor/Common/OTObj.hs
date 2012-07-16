module CEditor.Common.OTObj where

import Control.Monad(liftM, liftM2)
import Data.Binary

type ObjID = String
type ObjVersion = Integer

data ObjData = TextObj String
             | TreeObj ObjData ObjData
             | ErrorObj
             deriving (Eq, Ord, Show)

data Obj = Obj ObjID ObjVersion ObjData
         deriving (Eq, Ord, Show)

instance Binary ObjData where
  put (TextObj s) = do 
    put (1 :: Word16)
    put s
  put (TreeObj a b) = do 
    put (4 :: Word16)
    put a
    put b
  put ErrorObj = do put (0 :: Word16)
  get = do 
    tag <- get :: Get Word16
    case tag of
      1 -> liftM TextObj get
      4 -> liftM2 TreeObj get get
      _ -> return ErrorObj


data ActionDesc = InsertTextAction ObjID Integer String
                  -- NOTE: creating should not let users choose
                  -- objid's since they could choose one bad for
                  -- sharding. they should be chosen via the uuid
                  -- package.
                | CreateObjectAction ObjID ObjData
                deriving (Eq, Ord, Show)
-- TODO: make it Binary

actionObjIDs :: ActionDesc -> [ObjID]
actionObjIDs (InsertTextAction i _ _) = [i]
actionObjIDs (CreateObjectAction i _) = [i]

{-
after actionObjIDs is called, server gets the objects (if they exist). if they don't, they don't show up in the list.

applyActionToObjs will output a list of objects that should be upserted

in the case of create object, it does nothing if the object already exists, but
-}

insertText :: Integer -> String -> String -> String
insertText ix s2 s1 = let ix' = fromIntegral ix
                      in take ix' s1 ++ s2 ++ drop ix' s1

applyActionToObjs :: ActionDesc -> [Obj] -> [Obj]
applyActionToObjs (CreateObjectAction i d) [] = [Obj i 0 d]
applyActionToObjs (InsertTextAction i ix s) [Obj i0 v (TextObj s0)] = 
  if and [(i == i0), (ix >= 0)]
  then [Obj i0 (v+1) (TextObj (insertText ix s s0))]
  else []
applyActionToObjs _ _ = []



applyActionToAction :: ActionDesc -> ActionDesc -> [ActionDesc]
applyActionToAction (InsertTextAction i1 ix1 s1) (InsertTextAction i2 ix2 s2) =
  if and [i1 == i2, ix2 >= ix1]
  then [InsertTextAction i2 (ix2 + fromIntegral (length s1)) s2]
  else [InsertTextAction i2 ix2 s2]
applyActionToAction _ d = [d] -- otherwise, no change.


applyActionToActionQueue :: ActionDesc -> [ActionDesc] -> [ActionDesc]
applyActionToActionQueue a q = concat $ map (applyActionToAction a) q


