
-- | Gate constraints
--
-- Each gate occupies a single row (exlusively), and can have any (fixed) number 
-- of constraints.
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.Constraints where

--------------------------------------------------------------------------------

import Data.Array
import Data.Char

import Text.Show

import Goldilocks
import GoldilocksExt

import Gate.Base

--------------------------------------------------------------------------------
-- * Constraint expressions

-- | These index into a row + public input
data Var
  = SelV   Int        -- ^ selector variable
  | ConstV Int        -- ^ constant variable
  | WireV  Int        -- ^ wire variable
  | PIV    Int        -- ^ public input hash variable
  deriving (Eq,Ord,Show)

data Expr 
  = VarE   Var        -- ^ a variable
  | LitE   F          -- ^ constant literal
  | ScaleE F Expr     -- ^ linear scaling by a constant
  | ImagE  Expr       -- ^ multiplies by the field extension generator X
  | SumE   [Expr]     -- ^ sum of expressions
  | ProdE  [Expr]     -- ^ product of expressions
  | PowE   Expr Int   -- ^ exponentiation
  deriving (Eq) -- ,Show)

instance Show Expr where show = pretty

-- | Degree of the expression
exprDegree :: Expr -> Int
exprDegree = go where
  go expr = case expr of
    VarE   _   -> 1
    LitE   _   -> 0
    ScaleE _ e -> go e
    ImagE    e -> go e
    SumE   es  -> if null es then 0 else maximum (map go es)
    ProdE  es  -> sum (map go es)
    PowE   e n -> n * go e

instance Num Expr where
  fromInteger = LitE . fromInteger
  negate = negE
  (+) = addE
  (-) = subE
  (*) = mulE
  abs    = error "Expr/abs"
  signum = error "Expr/signum"

negE :: Expr -> Expr
negE (ScaleE s e) = ScaleE (negate s) e
negE e            = ScaleE (-1)       e

addE :: Expr -> Expr -> Expr
addE (SumE es) (SumE fs) = SumE (es++fs )
addE e         (SumE fs) = SumE (e : fs )
addE (SumE es) f         = SumE (es++[f])
addE e         f         = SumE [e,f]

subE :: Expr -> Expr -> Expr
subE e f = addE e (negate f)

sclE :: F -> Expr -> Expr
sclE s (ScaleE t e) = sclE (s*t) e
sclE s e            = ScaleE s e

mulE :: Expr -> Expr -> Expr
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

class Pretty a where
  prettyPrec :: Int -> a -> (String -> String)

pretty :: Pretty a => a -> String
pretty x = prettyPrec 0 x ""

instance Pretty F    where prettyPrec _ x = shows x
instance Pretty FExt where prettyPrec _ x = shows x

instance Pretty Var where
  prettyPrec _ v = case v of
    SelV   k -> showString ("s" ++ show k)
    ConstV k -> showString ("c" ++ show k)
    WireV  k -> showString ("w" ++ show k)
    PIV    k -> showString ("h" ++ show k)
instance Pretty Expr where
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

-- | List of all data (one "row") we need to evaluate a gate constraint
-- 
-- Typically this will be the evaluations of the column polynomials at @zeta@
data EvaluationVars a = MkEvaluationVars
  { local_selectors    :: Array Int a      -- ^ the selectors
  , local_constants    :: Array Int a      -- ^ the circuit constants 
  , local_wires        :: Array Int a      -- ^ the advice wires (witness)
  , public_inputs_hash :: [F]              -- ^ only used in @PublicInputGate@
  }
  deriving (Show)

--------------------------------------------------------------------------------
-- * Evaluation

class (Eq a, Show a, Num a, Fractional a) => EvalField a where
  fromGoldilocks :: Goldilocks -> a

instance EvalField F    where fromGoldilocks = id
instance EvalField FExt where fromGoldilocks = fromBase

evalExpr :: EvalField a => Expr -> EvaluationVars a -> a
evalExpr expr (MkEvaluationVars{..}) = go expr where
  go e = case e of
    VarE  v  -> case v of
      SelV   k -> local_selectors !  k
      ConstV k -> local_constants !  k
      WireV  k -> local_wires     !  k
      PIV    k -> fromGoldilocks (public_inputs_hash !! k)
    LitE  x  -> fromGoldilocks x
    SumE  es -> sum     (map go es)
    ProdE es -> product (map go es) 

--------------------------------------------------------------------------------
-- * Gate constraints

-- | Returns the (symbolic) constraints for the given gate
gateConstraints :: Gate -> [Expr]
gateConstraints gate = 
  case gate of

    -- `w[i] - c0*x[i]*y[i] - c1*z[i] = 0`
    ArithmeticGate num_ops 
      -> [ ww (j+3) - cc 0 * ww j * ww (j+1) - cc 1 * ww (j+2) | i<-range num_ops, let j = 4*i ]

    -- same but consecutive witness variables make up an extension field element
    ArithmeticExtensionGate num_ops 
      -> [ wwExt (j+6) - cc 0 * wwExt j * wwExt (j+2) - cc 1 * wwExt (j+4) | i<-range num_ops, let j = 8*i ]

    -- `sum b^i * limbs[i] - out = 0`, and `0 <= limb[i] < B` is enforced
    BaseSumGate num_limbs base
      -> let limb i = ww (i+1)
             horner = go 0 where go k = if k < num_limbs-1 then limb k + fromIntegral base * go (k+1) else limb k
             sum_eq = horner - ww 0
             range_eq i = ProdE [ limb i - fromIntegral k | k<-[0..base-1] ]
         in  sum_eq : [ range_eq i | i<-range num_limbs ]

    CosetInterpolationGate subgroup_bits coset_degree barycentric_weights 
      -> todo

    -- `c[i] - x[i] = 0`
    ConstantGate num_consts
      -> [ cc i - ww i | i <- range num_consts ]

    -- computes `out = base ^ (sum 2^i e_i)`
    -- order of witness variables: [ base, e[0],...,e[n-1], output, t[0]...t[n-1] ]
    ExponentiationGate num_power_bits 
      -> let base      = ww 0
             exp_bit i = ww (i+1)
             out       = ww (num_power_bits+1)
             tmp_val 0 = 1
             tmp_val i = ww (num_power_bits+1+i)
             cur_bit i = exp_bit (num_power_bits - 1 - i)
             eq i      = tmp_val (i-1) * (cur_bit i * base + 1 - cur_bit i) - tmp_val i
         in  [ eq i | i <- range num_power_bits ] ++ [ out - tmp_val (num_power_bits-1) ]

    -- lookups are handled specially, no constraints here
    LookupGate      num_slots lut_hash              -> []
    LookupTableGate num_slots lut_hash last_lut_row -> []

    -- `z[i] - c0*x[i]*y[i] = 0`, and two witness cells make up an extension field element
    MulExtensionGate num_ops 
      -> [ wwExt (j+4) - cc 0 * wwExt j * wwExt (j+2) | i<-range num_ops, let j = 6*i ]

    NoopGate -> []

    -- equality with "hardcoded" hash components
    PublicInputGate 
      -> [ hh i - ww i | i <- range 4 ]

    PoseidonGate hash_width -> case hash_width of
      12 -> todo -- poseidonGateConstraints
      k  -> error ( "gateConstraints/PoseidonGate: unsupported width " ++ show k)

    PoseidonMdsGate hash_width -> case hash_width of
      12 -> todo -- poseidonMdsGateConstraints
      k  -> error ( "gateConstraints/PoseidonMdsGate: unsupported width " ++ show k)

    RandomAccessGate num_bits num_copies num_extra_constants 
      -> todo

    ReducingGate num_coeffs 
      -> todo

    ReducingExtensionGate num_coeffs 
      -> todo

    UnknownGate name -> error ("gateConstraints: unknown gate `" ++ name ++ "`")

  where
 
    todo = error $ "gateConstraints: gate `" ++ takeWhile isAlpha (show gate) ++ "` not yet implemented"

    range k = [0..k-1]

    ww i = VarE (WireV  i)              -- witness variable
    cc i = VarE (ConstV i)              -- constant variable
    hh i = VarE (PIV    i)              -- public input hash component
    
    wwExt i = ww i + ImagE (ww (i+1))   -- use two consecutive variables as an extension field element

--------------------------------------------------------------------------------