{-# OPTIONS_GHC -F -pgmF htfpp #-}

import System.Environment ( getArgs )
import System.Exit ( exitWith )
import Test.Framework

test_equality = assertEqual "foo" "foo"

main = htfMain htf_thisModulesTests