

module Main where

import Haste.DOM
import Dom.Templ
import Data.Binary(encode, decode)

import CEditor.Common.OTObj




main :: IO ()
main = do
  let someTempl = RawTempl "asdf"
  let o = TextObj "pre"
  let b = encode o
  let o' = decode b
  let t = case o' of
        TextObj s -> s
        _ -> "a"
  e <- newElem t
  withElem "body" (addChild e)
  return ()










