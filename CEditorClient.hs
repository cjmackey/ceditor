

module Main where

import Haste
import Haste.DOM





main :: IO ()
main = do
  elem <- newElem "pre"
  withElem "body" (addChild elem)
  return ()










