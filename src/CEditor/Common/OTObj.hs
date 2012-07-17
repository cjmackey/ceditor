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
  put ErrorObj = put (0 :: Word16)
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

after actionObjIDs is called, server gets the objects (if they
exist). if they don't, they don't show up in the list.

applyActionToObjs will output a list of objects that should be upserted

in the case of create object, it does nothing if the object already
exists, but outputs the object to be created otherwise.

TODO: refactor to return (ActionDesc, [Obj]) so that we can wrap
security violations in a new ActionDesc subtype which does nothing, as
well as mark the ActionDesc with updated versions (ie, it should later
contain the version of the objects it just modified).

-}

insertText :: Integer -> String -> String -> String
insertText ix s2 s1 = let ix' = fromIntegral ix
                      in take ix' s1 ++ s2 ++ drop ix' s1

applyActionToObjs :: ActionDesc -> [Obj] -> [Obj]
applyActionToObjs (CreateObjectAction i d) [] = [Obj i 0 d]
applyActionToObjs (InsertTextAction i ix s) [Obj i0 v (TextObj s0)] 
  | i == i0 = [Obj i0 (v+1) (TextObj (insertText ix s s0))]
applyActionToObjs _ _ = []

{-

NOTE: applyActionToAction should be used only to apply actions that
have already been applied, i case of security violations

TODO: this should also update the version of the ActionDesc

-}

applyActionToAction :: ActionDesc -> ActionDesc -> [ActionDesc]
applyActionToAction (InsertTextAction i1 ix1 s1) (InsertTextAction i2 ix2 s2)
  | and [i1 == i2, ix2 >= ix1] = [InsertTextAction i2 (ix2 + fromIntegral (length s1)) s2]
applyActionToAction _ d = [d] -- otherwise, no change.


applyActionToActionQueue :: ActionDesc -> [ActionDesc] -> [ActionDesc]
applyActionToActionQueue a = concatMap (applyActionToAction a)


