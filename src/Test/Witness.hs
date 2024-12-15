
-- | We can test the gate constraints on actual witness rows during development

{-# LANGUAGE StrictData, DeriveGeneric, DeriveAnyClass, RecordWildCards #-}
module Test.Witness where

--------------------------------------------------------------------------------

import Data.Array

import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as L

import Algebra.Goldilocks
import Algebra.GoldilocksExt

import Gate.Base
import Gate.Vars
import Gate.Computation
import Gate.Constraints

import Misc.Pretty

--------------------------------------------------------------------------------

-- | As exported into JSON by our Plonky2 fork
data Witness = MkWitness
  { gates             :: [String]
  , selector_vector   :: [Int]
  , selector_columns  :: [[F]]
  , constants_columns :: [[F]]
  , matrix            :: [[F]]
  }
  deriving (Show,Generic,ToJSON,FromJSON)

matrixRow :: [[F]] -> Int -> [F]
matrixRow matrix i = map (!!i) matrix

witnessRow :: Witness -> Int -> EvaluationVars F
witnessRow (MkWitness{..}) i = MkEvaluationVars
  { local_selectors = toArray (matrixRow selector_columns  i)
  , local_constants = toArray (matrixRow constants_columns i)
  , local_wires     = toArray (matrixRow matrix            i)
  , public_inputs_hash = []
  }
  where
    toArray xs = listArray (0, length xs - 1) xs

loadWitness :: FilePath -> IO Witness
loadWitness fpath = do
  txt <- L.readFile fpath
  case decode txt of
    Just w  -> return w
    Nothing -> fail "loadWitness: cannot parse JSON witness"

--------------------------------------------------------------------------------

test_fibonacci = do

  witness <- loadWitness "../json/fibonacci_witness.json"

  let arith_row = witnessRow witness 0
  let posei_row = witnessRow witness 5
  let pubio_row = witnessRow witness 6
  let const_row = witnessRow witness 7

  let arith_prg = gateProgram (ArithmeticGate 20)
  let posei_prg = gateProgram (PoseidonGate   12)
  let pubio_prg = gateProgram (PublicInputGate  )
  let const_prg = gateProgram (ConstantGate    2)

  let arith_evals = runStraightLine (fmap fromBase arith_row) arith_prg  
  let posei_evals = runStraightLine (fmap fromBase posei_row) posei_prg
  let const_evals = runStraightLine (fmap fromBase const_row) const_prg

  putStrLn $ "number of constraints in ArithmeticGate  = " ++ show (length arith_evals)
  putStrLn $ "number of constraints in PosiedonGate    = " ++ show (length posei_evals)
  -- putStrLn $ "number of constraints in PublicInputGate = " ++ show (length pubio_evals)
  putStrLn $ "number of constraints in ConstGate       = " ++ show (length const_evals)

  putStrLn $ "number of operations in ArithmeticGate  = " ++ show (straightLineOperCount arith_prg)
  putStrLn $ "number of operations in PosiedonGate    = " ++ show (straightLineOperCount posei_prg)
  putStrLn $ "number of operations in PublicInputGate = " ++ show (straightLineOperCount pubio_prg)
  putStrLn $ "number of operations in ConstGate       = " ++ show (straightLineOperCount const_prg)

  print arith_evals
  print posei_evals
  print const_evals

  -- printStraightLine posei_prg
  
--------------------------------------------------------------------------------

main :: IO
main = test_fibonacci
