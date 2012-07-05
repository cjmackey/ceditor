module Tests.TemplParser where

import Test.HUnit
import Dom.TemplParser
import Data.Either

testTemplParser :: IO ()
testTemplParser = do
  [[Raw "asdf"]] @=? rights [parseHierarchy "x" "asdf"]
  [[]] @=? rights [parseHierarchy "x" ""]
  [[Eval "x-1" "asdf"]] @=? rights [parseHierarchy "x" "<%=asdf%>"]
  [[Eval "x-1" "asdf", Raw "blah"]] @=? rights [parseHierarchy "x" "<%=asdf%>blah"]
  [[Raw "blah", Eval "x-2" "asdf"]] @=? rights [parseHierarchy "x" "blah<%=asdf%>"]
  [[Raw "bl", Eval "x-2" "asdf", Raw "ah"]] @=? rights [parseHierarchy "x" "bl<%=asdf%>ah"]
  [[Eval "x-1" "asdf", Eval "x-2" "blah"]] @=? rights [parseHierarchy "x" "<%=asdf%><%=blah%>"]






