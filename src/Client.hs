

module Main where

import Haste.DOM
import Dom.Templ()




main :: IO ()
main = do
  e <- newElem "pre"
  withElem "body" (addChild e)
  return ()










