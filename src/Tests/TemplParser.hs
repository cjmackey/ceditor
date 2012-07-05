{-# LANGUAGE TemplateHaskell #-}

module Tests.TemplParser(testGroup) where

import Test.Framework.TH
import Test.Framework.Providers.HUnit
import Test.HUnit
import Dom.TemplTypes
import Dom.TemplParser
import Data.Either

{-# ANN module "HLint: ignore Use camelCase" #-}

checkPH :: [InchoateTempl] -> ID -> String -> IO ()
checkPH t i s = [t] @=? rights [parseHierarchy i s]

case_blank = checkPH [] "x" ""
case_raw1 = checkPH [Raw "asdf"] "" "asdf"
case_raw2 = checkPH [Raw "asdf"] "x" "asdf"
case_eval1 = checkPH [Eval "x-1" "span" "asdf"] "x" "<%=asdf%>"
case_eval2 = checkPH [Eval "x-1" "span" "asdf", Eval "x-2" "a" "blah"] "x" "<%=asdf%><%a=blah%>"
case_raw_eval = checkPH [Raw "blah", Eval "x-2" "span" "asdf"] "x" "blah<%=asdf%>"
case_eval_raw = checkPH [Eval "x-1" "div" "asdf", Raw "blah"] "x" "<%div=asdf%>blah"

testGroup = $(testGroupGenerator)
