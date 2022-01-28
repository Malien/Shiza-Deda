module Substitution where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import Term (Term (..), Var)

newtype Substitution a = Substitution (Map Var (Term a)) deriving (Eq)

instance (Show a) => Show (Substitution a) where
  show (Substitution mapping) =
    "{ "
      ++ ( unwords
             . fmap (\(var, sub) -> show var ++ " => " ++ show sub)
             . Map.toList
             $ mapping
         )
      ++ " }"

compose :: Substitution a -> Substitution a -> Substitution a
compose (Substitution a) sb@(Substitution b) = Substitution $ Map.union (Map.map (apply sb) a) b

empty = Substitution Map.empty

singleton key value = Substitution $ Map.singleton key value

(Substitution mapping) !? var = mapping Map.!? var

fromList = Substitution . Map.fromList

apply :: Substitution a -> Term a -> Term a
apply _ t@(Constant _) = t
apply sub t@(Variable var) = fromMaybe t (sub !? var)
apply sub (Application function arguments) = Application function $ apply sub <$> arguments
