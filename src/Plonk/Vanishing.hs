
-- | Evaluate all the Plonk constraints.
--
-- The following constraints are combined via powers of alpha:
--  
-- * boundary value constraints for the permutation running products (1 per challenge round)
-- 
-- * running product consistency constraints (10 per challenge round, where @10 = #routed wires / max_degree@)
--
-- * all lookup terms (many per challenge round)
--
-- * combined gate constraints (123 by default as the Poseidon gate is the largest)
--
-- This is done as many times as there are rounds, with different alphas (chosen from the base field).
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Plonk.Vanishing where

--------------------------------------------------------------------------------

import Data.Array
import Data.List ( foldl' , zipWith4 )

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Poly

import Challenge.Verifier

import Hash.Digest
import Hash.Sponge

import Gate.Computation
import Gate.Constraints
import Gate.Selector

import Plonk.Lookups

import Types
import Misc.Aux

--------------------------------------------------------------------------------

-- | We combine the same things with all the challenge round alphas... 
-- (Plonky2 is full with very strange design decisions...)
--
evalCombinedPlonkConstraints :: CommonCircuitData -> ProofWithPublicInputs -> ProofChallenges -> [FExt]
evalCombinedPlonkConstraints common proof challenges = final where
  constraints = evalAllPlonkConstraints common proof challenges 
  final = [ combineWithPowersOfAlpha alpha constraints | alpha <- plonk_alphas challenges ]

-- | @sum [ alpha^k * x_k ]@
combineWithPowersOfAlpha :: F -> [FExt] -> FExt
combineWithPowersOfAlpha alpha xs = foldl' f 0 (reverse xs) where
  f !acc !x = x + scaleExt alpha acc

--------------------------------------------------------------------------------

evalAllPlonkConstraints :: CommonCircuitData -> ProofWithPublicInputs -> ProofChallenges -> [FExt]
evalAllPlonkConstraints 
    common_data@(MkCommonCircuitData{..}) 
    prf_with_inp@(MkProofWithPublicInputs{..}) 
    challenges@(MkProofChallenges{..}) = finals 
  where

    finals = concat
      [ zs1
      , concat pp_checks
      , lookup_checks
      , gates
      ]

    lookup_checks = if null circuit_luts 
      then [] 
      else evalLookupEquations common_data lookupSelectors openings challenges

    MkProof{..}       = the_proof
    MkOpeningSet{..}  = openings
    
    selcfg@(MkSelectorConfig{..}) = getSelectorConfig common_data
    MkConstantColumns{..}         = splitConstantColumns selcfg opening_constants

    nn      = fri_nrows circuit_fri_params
    maxdeg  = circuit_quotient_degree_factor
    pi_hash = sponge public_inputs
 
    -- gate constraints
    eval_vars  = toEvaluationVars common_data pi_hash openings
    gate_prgs  = map gateProgram circuit_gates 
    sel_values = evalGateSelectors circuit_selectors_info gateSelectors 
    unfiltered = map (runStraightLine eval_vars) gate_prgs
    filtered   = zipWith (\s cons -> map (*s) cons) sel_values unfiltered
    gates      = combineFilteredGateConstraints filtered

    -- permutation constraints
    zs1 = [ evalLagrange0 nn plonk_zeta * (z-1) | z <- opening_plonk_zs ]
    pp_chunks = partition circuit_num_partial_products opening_partial_products
    pp_checks = zipWith4 evalPartialProducts 
      opening_plonk_zs 
      opening_plonk_zs_next 
      (zip plonk_betas plonk_gammas) 
      pp_chunks

    evalPartialProducts :: FExt -> FExt -> (F,F) -> [FExt] -> [FExt]
    evalPartialProducts z znext (beta,gamma) pp_chunk = zipWith3 f (pairs current) numers denoms where
      numers  = partition maxdeg $ zipWith (\koset w  -> w + scaleExt (beta*koset) plonk_zeta + fromBase gamma) circuit_k_is         opening_wires 
      denoms  = partition maxdeg $ zipWith (\sigma w  -> w + scaleExt  beta        sigma      + fromBase gamma) opening_plonk_sigmas opening_wires 
      current = [z] ++ pp_chunk ++ [znext]
      f :: (FExt,FExt) -> [FExt] -> [FExt] -> FExt
      f (prev,next) numer denom = prev * product numer - next * product denom

--------------------------------------------------------------------------------

-- | Each gate has some number of constrains (depending on which gate it is).
-- These are combined \"verticaly\" by simple addition, so that each term in the 
-- resulting list of summed constraints is a sum of constraints from distinct gates,
-- at most 1 per gate.
--
-- This look suspicious at first glance: normally we never just add together constraints!
-- However this looks safe in this case, because the selectors ensure that when evaluating
-- at a particular row of the witness, each such sum can have at most 1 nonzero summand

combineFilteredGateConstraints :: [[FExt]] -> [FExt]
combineFilteredGateConstraints = foldl1 (longZipWith 0 0 (+)) 

--------------------------------------------------------------------------------

toEvaluationVars :: CommonCircuitData -> Digest -> OpeningSet -> EvaluationVars FExt
toEvaluationVars common_data pi_hash (MkOpeningSet{..}) = 
  MkEvaluationVars
    { local_selectors    = listToArray gateSelectors  
    , local_lkp_sels     = listToArray lookupSelectors
    , local_constants    = listToArray gateConstants  
    , local_wires        = listToArray opening_wires
    , public_inputs_hash = digestToList pi_hash 
    }
  where
    selcfg = getSelectorConfig common_data
    MkConstantColumns{..} = splitConstantColumns selcfg opening_constants

--------------------------------------------------------------------------------

