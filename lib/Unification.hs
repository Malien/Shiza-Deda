module Unification (unify) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Substitution (Substitution, (!?))
import qualified Substitution as Sub
import Term (Term (..), Var (Var))

occurs :: Var -> Term a -> Bool
occurs _ (Constant _) = False
occurs var (Variable other) = var == other
occurs var (Application _ arguments) = any (occurs var) arguments

unify :: (Eq a) => Term a -> Term a -> Maybe (Substitution a)
unify (Constant a) (Constant b)
  | a == b = Just Sub.empty
  | otherwise = Nothing
unify (Variable a) (Variable b)
  | a == b = Just Sub.empty
unify (Variable var) term
  | occurs var term = Nothing
  | otherwise = Just $ Sub.singleton var term
unify termA termB@(Variable var) = unify termB termA
unify (Constant _) (Application _ _) = Nothing
unify (Application _ _) (Constant _) = Nothing
unify (Application nameA _) (Application nameB _)
  | nameA /= nameB = Nothing
unify (Application _ argsA) (Application _ argsB) =
  unifyArgs (NonEmpty.toList argsA) (NonEmpty.toList argsB)

unifyArgs [] [] = Just Sub.empty
unifyArgs [] _ = Nothing
unifyArgs _ [] = Nothing
unifyArgs (x : xs) (y : ys) = do
  subs1 <- unify x y
  subs2 <- unifyArgs (map (Sub.apply subs1) xs) (map (Sub.apply subs1) ys)
  return $ Sub.compose subs1 subs2
