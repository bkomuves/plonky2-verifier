
-- | We have to describe the contraints as computations with local definitions
-- Without local definitions the equations would just blow up 
-- in the case of the Poseidon gate for example
--

{-# LANGUAGE StrictData, DeriveFunctor, GADTs, RecordWildCards #-}
module Gate.Computation where

--------------------------------------------------------------------------------

import Prelude hiding ( (^) )

import Control.Applicative
import Control.Monad

import Data.Array
import Data.List
import Text.Show

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Expr

import Gate.Vars
import Misc.Pretty

--------------------------------------------------------------------------------
-- * Operational monad

data Instr var a where
  Let    :: String -> Expr var -> Instr var var
  Commit :: Expr var -> Instr var ()
  
data Program instr a where
  Bind   :: Program instr a -> (a -> Program instr b) -> Program instr b
  Return :: a -> Program instr a
  Instr  :: instr a -> Program instr a

instance Functor (Program instr) where
  fmap = liftM

instance Applicative (Program instr) where
  (<*>) = ap
  pure  = Return

instance Monad (Program instr) where
  (>>=)  = Bind

type Var_     = Var PlonkyVar
type Expr_    = Expr     Var_
type Def_     = LocalDef Var_
--type Instr_ a = Instr    Var_ a

--------------------------------------------------------------------------------

-- | Our computation monad
type Compute a = Program (Instr Var_) a

let_ :: String -> Expr Var_ -> Compute (Expr Var_)
let_ name rhs = VarE <$> Instr (Let name rhs)

commit :: Expr Var_ -> Compute ()
commit what = Instr (Commit what)

commitList :: [Expr Var_] -> Compute ()
commitList = mapM_ commit

--------------------------------------------------------------------------------
-- | Straightline programs

data LocalDef v 
  = MkLocalDef Int String (Expr v)
  deriving (Eq,Show)

instance Pretty v => Pretty (LocalDef v) where
  prettyPrec _ (MkLocalDef k name rhs) = showString ("_" ++ name ++ show k) . showString " := " . prettyPrec 0 rhs

-- | A straightline program encoding the computation of constraints
data StraightLine = MkStraightLine 
  { localdefs :: [LocalDef Var_]         -- ^ local definitions, in reverse order
  , commits   :: [Expr_]                 -- ^ committed constraints, in reverse order
  , counter   :: Int                     -- ^ fresh variable counter
  }
  deriving Show

emptyStraightLine :: StraightLine
emptyStraightLine = MkStraightLine [] [] 0

printStraightLine :: StraightLine -> IO ()
printStraightLine (MkStraightLine{..}) = do
  forM_ (reverse localdefs) $ \def  -> putStrLn (pretty def)
  forM_ (reverse commits  ) $ \expr -> putStrLn $ "constraint 0 == " ++ (pretty expr)

compileToStraightLine :: Compute () -> StraightLine
compileToStraightLine = fst . go emptyStraightLine where
  go :: StraightLine -> Compute a -> (StraightLine,a) 
  go state instr = case instr of
     Return x       -> (state,x)
     Bind this rest -> let (state',x) = go state this in go state' (rest x)
     Instr this     -> case state of
       MkStraightLine{..} -> case this of
         Commit what  -> let state' = MkStraightLine localdefs (what:commits) counter
                         in  (state', ())
         Let name rhs -> let def    = MkLocalDef counter name rhs
                             state' = MkStraightLine (def:localdefs) commits (counter+1)
                         in  (state', LocalVar counter name)

--------------------------------------------------------------------------------

type Scope a = IntMap a

emptyScope :: Scope a
emptyScope = IntMap.empty

-- | Run a \"straightline program\", resulting in list of contraints evaluations
runStraightLine :: EvaluationVars FExt -> StraightLine -> [FExt]
runStraightLine = runStraightLine' emptyScope

runStraightLine' :: Scope FExt -> EvaluationVars FExt -> StraightLine -> [FExt]
runStraightLine' iniScope vars (MkStraightLine{..}) = result where
  finalScope = foldl' worker iniScope (reverse localdefs)
  result = evalConstraints finalScope vars (reverse commits) 
  worker !scope (MkLocalDef i _ rhs) = IntMap.insert i (evalConstraint scope vars rhs) scope

--------------------------------------------------------------------------------
-- * Evaluation

type Constraint = Expr_

-- | List of all data (one "row") we need to evaluate a gate constraint
-- 
-- Typically this will be the evaluations of the column polynomials at @zeta@
data EvaluationVars a = MkEvaluationVars
  { local_selectors    :: Array Int a      -- ^ the selectors
  , local_constants    :: Array Int a      -- ^ the circuit constants 
  , local_wires        :: Array Int a      -- ^ the advice wires (witness)
  , public_inputs_hash :: [F]              -- ^ only used in @PublicInputGate@
  }
  deriving (Show,Functor)

evalConstraint :: Scope FExt -> EvaluationVars FExt -> Constraint -> FExt
evalConstraint scope (MkEvaluationVars{..}) expr = evalExprWith f expr where
  f var = case var of
    LocalVar i n -> case IntMap.lookup i scope of 
      Just y  -> y 
      Nothing -> error $ "variable _" ++ n ++ show i ++ " not in scope"
    ProofVar v  -> case v of
      SelV   k -> local_selectors ! k
      ConstV k -> local_constants ! k
      WireV  k -> local_wires     ! k
      PIV    k -> fromBase (public_inputs_hash !! k)
 
evalConstraints :: Scope FExt -> EvaluationVars FExt -> [Constraint] -> [FExt]
evalConstraints scope vars = map (evalConstraint scope vars) 

--------------------------------------------------------------------------------
