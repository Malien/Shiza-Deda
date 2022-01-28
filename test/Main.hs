module Main where

import HTermsTest (testHTerms)
import Test.Tasty (defaultMain, testGroup)
import UnificationTest (testUnification)

tests = testGroup "Shiza" [testUnification, testHTerms]

main = defaultMain tests