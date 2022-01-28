module UnificationTest (testUnification) where

import qualified Data.List.NonEmpty as NonEmpty
import Unification (unify)
import qualified Substitution
import Term (Term (..), Var (Var))
import Test.Tasty (testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

a = Constant "a"
b = Constant "b"
x = Var "x"
y = Var "y"
z = Var "z"
varX = Variable x
varY = Variable y
varZ = Variable z
f = Application "f" . NonEmpty.fromList
g = Application "g" . NonEmpty.fromList

-- Table from https://en.wikipedia.org/wiki/Unification_(computer_science)#Examples_of_syntactic_unification_of_first-order_terms
substitutions =
  [ (a                  , a           , Substitution.empty                        ),
    (varX               , varX        , Substitution.empty                        ),
    (a                  , varX        , Substitution.singleton x a                ),
    (varX               , varY        , Substitution.singleton x varY             ),
    (f [a, varX]        , f [a, b]    , Substitution.singleton x b                ),
    (f [varX]           , f [varY]    , Substitution.singleton x varY             ),
    (f [g [varX]]       , f [varY]    , Substitution.singleton y (g [varX])       ),
    (f [g [varX], varX] , f [varY, a] , Substitution.fromList [(x, a), (y, g [a])])
  ]

ununifiable =
  [ (a        , b             ),
    (f [a]    , g [a]         ),
    (f [varX] , g [varY]      ),
    (f [varX] , g [varY, varZ]),
    (varX     , f [varX]      ),

    (f [b]    , a             ),
    (f [varX] , a             )
  ]

unifiableTestCase (a, b, res) =
  testCase ("Unification of " ++ show a ++ " and " ++ show b ++ " produces valid substitution of " ++ show res) $
    unify a b @?= Just res

ununifiableTestCase (a, b) =
  testCase ("Unification of " ++ show a ++ " and " ++ show b ++ " does not produce a valid substitution") $
    unify a b @?= Nothing

testPossibleSubstitutions = testGroup "Unifiable examples" $ map unifiableTestCase substitutions

testUninifiableSubstitutions = testGroup "Un-unifiable examples" $ map ununifiableTestCase ununifiable

testUnification = testGroup "Unifications" [testPossibleSubstitutions, testUninifiableSubstitutions]
