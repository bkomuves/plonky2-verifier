
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

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Expr

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

instance Pretty Var where
  prettyPrec _ v = case v of
    SelV   k -> showString ("s" ++ show k)
    ConstV k -> showString ("c" ++ show k)
    WireV  k -> showString ("w" ++ show k)
    PIV    k -> showString ("h" ++ show k)

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

evalExpr :: Expr Var -> EvaluationVars FExt -> FExt
evalExpr expr (MkEvaluationVars{..}) = evalExprWith f expr where
  f v = case v of
    SelV   k -> local_selectors !  k
    ConstV k -> local_constants !  k
    WireV  k -> local_wires     !  k
    PIV    k -> fromBase (public_inputs_hash !! k)
 
--------------------------------------------------------------------------------
-- * Gate constraints

-- | Returns the (symbolic) constraints for the given gate
gateConstraints :: Gate -> [Expr Var]
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