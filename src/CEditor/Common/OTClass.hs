{-# LANGUAGE MultiParamTypeClasses #-}
module CEditor.Common.OTClass where

import Prelude hiding (drop, foldl)
import Data.Foldable (foldl)
import Data.Map (Map, maxViewWithKey, splitLookup, foldlWithKey, insert)
import qualified Data.Map
import Data.Sequence (Seq, drop, (|>))
import qualified Data.Sequence

type Version = Rational
type UserID = String

class Versioned a where
  version :: a -> Version
  setVersion :: Version -> a -> a

class UserTagged a where
  userID :: a -> UserID

class (Versioned t, UserTagged t) => Transformation t where
  transformTransformation :: t -> t -> t

class (Versioned o, Versioned t, UserTagged t, Transformation t) => OT o t where
  transformObject :: t -> o -> o

-- TODO: figure out what to do about advancing object version #
data (OT o t) => 
     ObjectContainer o t = ObjectContainer { snapshots :: Map Version o
                                           , current :: o
                                           , tranLog :: Map Version t }
                         deriving (Show)
newObjectContainer o = ObjectContainer { snapshots = Data.Map.singleton (version o) o
                                       , current = o
                                       , tranLog = Data.Map.empty }
data (OT o t) =>
     ClientObjectContainer o t = ClientObjectContainer { selfID :: UserID
                                                       , serverView :: o
                                                       , currentView :: o
                                                       , clientLog :: Seq t }
                               deriving (Show)
instance (OT o t) => UserTagged (ClientObjectContainer o t) where
  userID = selfID

newClientObjectContainer uid o = ClientObjectContainer { selfID = uid
                                                       , serverView = o
                                                       , currentView = o
                                                       , clientLog = Data.Sequence.empty }

splitGTE :: (Ord k) => k -> Map k a -> Map k a
splitGTE k m = case splitLookup k m of
  (_, Just mid, m') -> insert k mid m'
  (_, Nothing, m') -> m'

{-

   to be called when, for example, all subscribers have announced that
   they have advanced their internal version.

-}
advanceLog :: (OT o t) => Version -> ObjectContainer o t -> ObjectContainer o t
advanceLog newStart oc = oc { tranLog = splitGTE newStart (tranLog oc) }


-- returns the modified transformation to send to subscribers as well
-- as the modified object container.
appendTransformation :: (OT o t) => t -> ObjectContainer o t -> (t, ObjectContainer o t)
appendTransformation t oc = (t'', oc { current = transformObject t'' (current oc)
                                     , tranLog = log1 })
  where log0 = tranLog oc
        t' = multitrans (splitGTE (version t) log0) t
        t'' = setVersion (case maxViewWithKey log0 of
                             Just ((k,_), _) -> 1 + fromIntegral (ceiling k :: Integer)
                             Nothing -> version $ current oc) t'
        log1 = insert (version t'') t'' log0

multitrans :: (Transformation t) => Map Version t -> t -> t
multitrans s t = foldlWithKey (\t1 _ t0 -> if userID t1 /= userID t0
                                           then transformTransformation t0 t1
                                           else t1) t s

-- returns the transformation to send to the server as well as the
-- modified object container.
applyClientTransformation :: (OT o t) => t -> ClientObjectContainer o t 
                             -> (t, ClientObjectContainer o t)
applyClientTransformation t coc = (setVersion (version (serverView coc)) t,
                                   coc { currentView = transformObject t (currentView coc)
                                       , clientLog = clientLog coc |> t })

recvServerTransformation :: (OT o t) => t -> ClientObjectContainer o t 
                            -> ClientObjectContainer o t
recvServerTransformation t coc = if userID t == selfID coc
                                 then coc { serverView = obj'
                                          , clientLog = drop 1 clog }
                                 else coc''
  where obj' = transformObject t (serverView coc)
        coc' = coc { serverView = obj'
                   , currentView = obj'
                   , clientLog = Data.Sequence.empty }
        clog = clientLog coc
        clog' = fmap (transformTransformation t) clog
        coc'' = foldl (\a b -> snd $ applyClientTransformation b a) coc' clog'



