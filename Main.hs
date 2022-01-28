module Main where

import Data.Maybe (isJust)
import Substitution (Substitution (Substitution))
import qualified Substitution
import System.Environment (getArgs)
import Term (Term, hTerm)
import Unification (unify)

bench :: Int -> IO (Maybe (Substitution ()))
bench order | order <= 10 = do
  print a
  print b
  let res = unify a b
  print res
  return res
  where
    a, b :: Term ()
    (a, b) = hTerm order
bench order = do
  putStrLn $ if isJust res then "Unification exists" else "Unification does not exist"
  return res
  where
    (a, b) = hTerm order
    res = unify a b

main = do
  [orderStr] <- getArgs
  let order = read orderStr
  putStrLn $ "Unification of H-terms of order " ++ show order ++ ":"
  res <- bench order
  return ()