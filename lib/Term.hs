module Term where

import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty

newtype Var = Var String deriving (Eq, Ord)

instance Show Var where
  show (Var name) = name

data Term a
  = Constant a
  | Variable Var
  | Application String (NonEmpty (Term a))
  deriving (Eq)

instance (Show a) => Show (Term a) where
  show (Constant a) = show a
  show (Variable var) = show var
  show (Application name args) = name ++ "(" ++ (intercalate ", " . map show . NonEmpty.toList $ args) ++ ")"

-- 0: h(y0)
--    h(x0)
-- 1: h(x1, f(y0, y0), y1)
--    h(f(x0, x0), y1, x1)
-- 2: h(x1, x2, f(y0, y0), f(y1, y1), y2)
--    h(f(x0, x0), f(x1, x1), y1, y2, x2)

h = Application "h" . NonEmpty.fromList
f a b = Application "f" (a :| [b])
f1 x = f x x
x order = Variable . Var $ "x" ++ show order
y order = Variable . Var $ "y" ++ show order
xs = map x [0 ..]
ys = map y [0 ..]
fxs = map f1 xs
fys = map f1 ys

hTerm :: Int -> (Term a, Term a)
hTerm x | x < 0 = undefined
-- hTerm 0 = (h [y 0], h [x 0])
hTerm order =
  ( h (tail (take len xs) ++ take (len - 1) fys ++ [y order]),
    h (take (len - 1) fxs ++ tail (take len ys) ++ [x order])
  )
  where 
    len = order + 1