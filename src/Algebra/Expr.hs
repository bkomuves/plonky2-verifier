
-- | Polynomial expressions
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Algebra.Expr where

--------------------------------------------------------------------------------

import Prelude hiding ( (^) )

import Data.Array
import Data.Char

import Text.Show

import Algebra.Goldilocks
import Algebra.GoldilocksExt

import Gate.Base
import Misc.Pretty

--------------------------------------------------------------------------------
-- * Polynomial expressions

data Expr v
  = VarE v                    -- ^ a variable
  | LitE F                    -- ^ constant literal
  | AddE (Expr v) (Expr v)    -- ^ addition
  | SubE (Expr v) (Expr v)    -- ^ subtraction
  | MulE (Expr v) (Expr v)    -- ^ multiplication
  | ImgE (Expr v)             -- ^ multiplies by the field extension generator X
  deriving (Eq,Show)

-- instance Pretty var => Show (Expr var) where show = pretty

-- | Degree of the expression
exprDegree :: Expr var -> Int
exprDegree = go where
  go expr = case expr of
    VarE  _    -> 1
    LitE  _    -> 0
    AddE e1 e2 -> max (go e1) (go e2)
    SubE e1 e2 -> max (go e1) (go e2)
    MulE e1 e2 -> go e1 + go e2
    ImgE e     -> go e

instance Num (Expr var) where
  fromInteger = LitE . fromInteger
  negate = negE
  (+) = AddE
  (-) = SubE
  (*) = MulE
  abs    = error "Expr/abs"
  signum = error "Expr/signum"

negE :: Expr var -> Expr var
negE e = SubE (LitE 0) e

{-
(^) :: Expr var -> Int -> Expr var
(^) = PowE

addE :: Expr var -> Expr var -> Expr var
addE = AddE 

mulE :: Expr var -> Expr var -> Expr var
mulE = MulE 
-}

--------------------------------------------------------------------------------
-- * pretty printing

instance Pretty var => Pretty (Expr var) where
  prettyPrec d expr = 
    case expr of
      VarE v     -> prettyPrec 0 v
      LitE x     -> prettyPrec 0 x
      AddE e1 e2 -> showParen (d > add_prec) $ prettyPrec add_prec e1 . showString " + " . prettyPrec (add_prec+1) e2
      SubE e1 e2 -> showParen (d > add_prec) $ prettyPrec add_prec e1 . showString " - " . prettyPrec (add_prec+1) e2
      MulE e1 e2 -> showParen (d > mul_prec) $ prettyPrec mul_prec e1 . showString " * " . prettyPrec (mul_prec+1) e2
      ImgE e     -> showParen (d > mul_prec) $ showString "X*" . (prettyPrec mul_prec e)
    where
      add_prec = 5
      mul_prec = 6
      -- pow_prec = 7
      -- intercalates sep = go where
      --   go []     = id
      --   go [x]    = x
      --   go (x:xs) = x . showString sep . go xs

--------------------------------------------------------------------------------
-- * Evaluation

{-
class (Eq a, Show a, Num a, Fractional a) => EvalField a where
  fromGoldilocks :: Goldilocks -> a

instance EvalField F    where fromGoldilocks = id
instance EvalField FExt where fromGoldilocks = fromBase
-}

evalExprWith :: (var -> FExt) -> Expr var -> FExt
evalExprWith evalVar expr = go expr where
  go e = case e of
    VarE v     -> evalVar v
    LitE x     -> fromBase x
    AddE e1 e2 -> go e1 + go e2
    SubE e1 e2 -> go e1 - go e2
    MulE e1 e2 -> go e1 * go e2
    ImgE e     -> (MkExt 0 1) * go e

--------------------------------------------------------------------------------
