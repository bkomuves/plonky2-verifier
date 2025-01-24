
-- | Variables appearing in gate constraints

module Gate.Vars where

--------------------------------------------------------------------------------

import Text.Show

import Algebra.GoldilocksExt
import Algebra.Expr
import Misc.Pretty

--------------------------------------------------------------------------------
-- * Constraint variables

-- | These index into a row + public input
data PlonkyVar
  = SelV    Int        -- ^ selector variable
  | LkpSelV Int        -- ^ lookup selector variable
  | ConstV  Int        -- ^ constant variable
  | WireV   Int        -- ^ wire variable
  | PIV     Int        -- ^ public input hash variable (technically these are constants, not variables)
  deriving (Eq,Ord,Show)

instance Pretty PlonkyVar where
  prettyPrec _ v = case v of
    SelV    k -> showString ("s" ++ show k)
    LkpSelV k -> showString ("l" ++ show k)
    ConstV  k -> showString ("c" ++ show k)
    WireV   k -> showString ("w" ++ show k)
    PIV     k -> showString ("h" ++ show k)

--------------------------------------------------------------------------------
-- * Variables

data Var v
  = LocalVar Int String      -- ^ a temporary variable
  | ProofVar v               -- ^ a proof variable (eg. witness, constant, selector)
  deriving (Eq,Show)

instance Pretty v => Pretty (Var v) where
  prettyPrec d var = case var of
    LocalVar k name -> showString ("_" ++ name ++ show k)
    ProofVar v      -> prettyPrec d v

--------------------------------------------------------------------------------
-- * Convenience

wire, cnst, hash :: Int -> Expr (Var PlonkyVar)
wire i = VarE $ ProofVar $ WireV  i       -- witness variable
cnst i = VarE $ ProofVar $ ConstV i       -- constant variable
hash i = VarE $ ProofVar $ PIV    i       -- public input hash component

-- use two consecutive variables as a _simulated_ extension field element
wireExt :: Int -> Ext (Expr (Var PlonkyVar))
wireExt i = MkExt (wire i) (wire (i+1))     

--------------------------------------------------------------------------------
