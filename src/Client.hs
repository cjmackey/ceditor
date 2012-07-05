

module Main where

import Haste.DOM





main :: IO ()
main = do
  e <- newElem "pre"
  withElem "body" (addChild e)
  return ()










