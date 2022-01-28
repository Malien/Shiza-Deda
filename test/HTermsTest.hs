module HTermsTest (testHTerms) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Term (Term (Application, Variable), Var (Var), hTerm)
import Test.Tasty (testGroup)
import Test.Tasty.HUnit (testCase, (@?=))

h = Application "h" . NonEmpty.fromList
f' a b = Application "f" (a :| [b])
f x = f' x x
x order = Variable . Var $ "x" ++ show order
y order = Variable . Var $ "y" ++ show order

-- 0: h(y0)
--    h(x0)
-- 1: h(x1, f(y0, y0), y1)
--    h(f(x0, x0), y1, x1)
-- 2: h(x1, x2, f(y0, y0), f(y1, y1), y2)
--    h(f(x0, x0), f(x1, x1), y1, y2, x2)

hTermsExamples :: [(Term (), Term ())]
hTermsExamples =
  [ ( h [y 0],
      h [x 0]
    ),
    ( h [x 1, f (y 0), y 1],
      h [f (x 0), y 1, x 1]
    ),
    ( h [x 1, x 2, f (y 0), f (y 1), y 2],
      h [f (x 0), f (x 1), y 1, y 2, x 2]
    )
  ]

hTermTestCase order res =
  testCase ("H terms of order " ++ show order ++ " are " ++ show res) $
    hTerm order @?= res

testHTerms = testGroup "H term examples" $ zipWith hTermTestCase [0 ..] hTermsExamples