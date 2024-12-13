
-- | Polynomial expressions
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Algebra.Expr where

--------------------------------------------------------------------------------

import Data.Array
import Data.Char

import Text.Show

import Algebra.Goldilocks
import Algebra.GoldilocksExt

import Gate.Base

--------------------------------------------------------------------------------
-- * Polynomial expressions

data Expr v
  = VarE   v              -- ^ a variable
  | LitE   F              -- ^ constant literal
  | ScaleE F (Expr v)     -- ^ linear scaling by a constant
  | ImagE  (Expr v)       -- ^ multiplies by the field extension generator X
  | SumE   [Expr v]       -- ^ sum of expressions
  | ProdE  [Expr v]       -- ^ product of expressions
  | PowE   (Expr v) Int   -- ^ exponentiation
  deriving (Eq) -- ,Show)

instance Pretty var => Show (Expr var) where show = pretty

-- | Degree of the expression
exprDegree :: Expr var -> Int
exprDegree = go where
  go expr = case expr of
    VarE   _   -> 1
    LitE   _   -> 0
    ScaleE _ e -> go e
    ImagE    e -> go e
    SumE   es  -> if null es then 0 else maximum (map go es)
    ProdE  es  -> sum (map go es)
    PowE   e n -> n * go e

instance Num (Expr var) where
  fromInteger = LitE . fromInteger
  negate = negE
  (+) = addE
  (-) = subE
  (*) = mulE
  abs    = error "Expr/abs"
  signum = error "Expr/signum"

negE :: Expr var -> Expr var
negE (ScaleE s e) = ScaleE (negate s) e
negE e            = ScaleE (-1)       e

addE :: Expr var -> Expr var -> Expr var
addE (SumE es) (SumE fs) = SumE (es++fs )
addE e         (SumE fs) = SumE (e : fs )
addE (SumE es) f         = SumE (es++[f])
addE e         f         = SumE [e,f]

subE :: Expr var -> Expr var -> Expr var
subE e f = addE e (negate f)

sclE :: F -> Expr var -> Expr var
sclE s (ScaleE t e) = sclE (s*t) e
sclE s e            = ScaleE s e

mulE :: Expr var -> Expr var -> Expr var
mulE (ScaleE s e) (ScaleE t f) = sclE (s*t) (mulE e f)
mulE (ScaleE s e) f            = sclE  s    (mulE e f)
mulE (LitE s) f                = sclE s f
mulE e            (LitE t)     = sclE t e
mulE e            (ScaleE t f) = sclE    t  (mulE e f)
mulE (ProdE es)   (ProdE fs)   = ProdE (es++fs )
mulE e            (ProdE fs)   = ProdE (e : fs )
mulE (ProdE es)   f            = ProdE (es++[f])
mulE e            f            = ProdE [e,f]

--------------------------------------------------------------------------------
-- * pretty printing

-- | TODO: maybe move this somewhere else
class Pretty a where
  prettyPrec :: Int -> a -> (String -> String)

pretty :: Pretty a => a -> String
pretty x = prettyPrec 0 x ""

instance Pretty F    where prettyPrec _ x = shows x
instance Pretty FExt where prettyPrec _ x = shows x

instance Pretty var => Pretty (Expr var) where
  prettyPrec d expr = 
    case expr of
      VarE   v   -> prettyPrec 0 v
      LitE   x   -> prettyPrec 0 x
      ScaleE s e -> prettyPrec 0 s . showString " * " . showParen (d > mul_prec) (prettyPrec mul_prec e)
      ImagE    e ->                  showString "X*"  . showParen (d > mul_prec) (prettyPrec mul_prec e)
      SumE   es  -> showParen (d > add_prec) $ intercalates " + " $ map (prettyPrec add_prec) es 
      ProdE  es  -> showParen (d > mul_prec) $ intercalates " * " $ map (prettyPrec mul_prec) es
      PowE   e k -> showParen (d > pow_prec) $ (prettyPrec pow_prec e) . showString ("^" ++ show k)
    where
      add_prec = 5
      mul_prec = 6
      pow_prec = 7
      intercalates sep = go where
        go []     = id
        go [x]    = x
        go (x:xs) = x . showString sep . go xs

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
    VarE   v   -> evalVar v
    LitE   x   -> fromBase x
    ScaleE s e -> fromBase s * go e
    ImagE    e -> (MkExt 0 1) * go e
    SumE  es   -> sum     (map go es)
    ProdE es   -> product (map go es) 
    PowE   e n -> powExt (go e) (fromIntegral n)

--------------------------------------------------------------------------------
