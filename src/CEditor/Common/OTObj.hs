module CEditor.Common.OTObj where

import Control.Monad(liftM, liftM2)
import Data.Binary

data Obj = TextObj String
         | TreeObj Obj Obj
         | ErrorObj
         deriving (Eq, Show)


instance Binary Obj where
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
