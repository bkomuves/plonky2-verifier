
-- | Check the combined Plonk equation @Q_alpha(zeta) * Z_X(zeta) = C_alpha(zeta)@
-- 
-- Note: there are @num_challenge_rounds@ number of these (usually 2 or 3); 
-- they are the same except with different combining @alpha@ coefficients.
--

{-# LANGUAGE RecordWildCards #-}
module Plonk.Verifier where

--------------------------------------------------------------------------------

import Data.Array
import Data.List ( foldl' , zipWith4 )

import Algebra.Goldilocks
import Algebra.GoldilocksExt

import Challenge.Verifier
import Plonk.Vanishing
import Plonk.FRI

import Hash.Digest

import Types
import Misc.Aux

--------------------------------------------------------------------------------

-- | Assuming valid FRI openings, this function checks the combined Plonk equations
checkCombinedPlonkEquations :: CommonCircuitData -> ProofWithPublicInputs -> ProofChallenges -> Bool
checkCombinedPlonkEquations common proof challenges = 
  and (checkCombinedPlonkEquations' common proof challenges) 

checkCombinedPlonkEquations' :: CommonCircuitData -> ProofWithPublicInputs -> ProofChallenges -> [Bool]
checkCombinedPlonkEquations' common proof_pis challenges = ok_list where
  
  maxdeg = circuit_quotient_degree_factor common :: Int
  nn     = circuit_nrows common                  :: Int
  zeta   = plonk_zeta challenges                 :: FExt
  zeta_n = powExt_ zeta nn                       :: FExt
  
  combined_evals  = evalCombinedPlonkConstraints common proof_pis challenges                   :: [FExt]
  quotient_evals  = map (reduceWithPowers zeta_n) quotient_chunks                               :: [FExt]
  quotient_chunks = partition maxdeg (opening_quotient_polys $ openings $ the_proof proof_pis) :: [[FExt]]

  ok_list = [ q * (zeta_n - 1) == c | (q,c) <- safeZip quotient_evals combined_evals ]

  -- sum [ u^i * q[i] | i<-[0..] ]
  reduceWithPowers :: FExt -> [FExt] -> FExt
  reduceWithPowers u chunk = foldl' f 0 (reverse chunk) where
    f !acc !x = x + u * acc

--------------------------------------------------------------------------------

verifyProof :: VerifierCircuitData -> ProofWithPublicInputs -> Bool
verifyProof vkey@(MkVerifierCircuitData{..}) pis@(MkProofWithPublicInputs{..}) = all_ok where

  common     = verifier_common
  challenges = proofChallenges common verifier_only pis

  all_ok = eqs_ok && fri_ok

  eqs_ok = checkCombinedPlonkEquations common pis challenges 
  fri_ok = checkFRIProof vkey the_proof challenges

--------------------------------------------------------------------------------
