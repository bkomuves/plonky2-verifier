
-- | Evaluation the constraints of the lookup argument

{-# LANGUAGE StrictData, RecordWildCards #-}
module Plonk.Lookups where

--------------------------------------------------------------------------------

import Data.Array
import Data.List ( foldl' )

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Poly

import Challenge.Verifier
import Gate.Selector

import Types
import Misc.Aux

-- import Debug.Trace
-- debug msg x y = trace (msg ++ ": " ++ show x) y

--------------------------------------------------------------------------------

data LookupSelector
  = TransSre          -- ^ @TransSre@ is for Sum and RE transition constraints.
  | TransLdc          -- ^ @TransLdc@ is for LDC transition constraints.
  | InitSre           -- ^ @InitSre@  is for the initial constraint of Sum and Re.
  | LastLdc           -- ^ @LastLdc@  is for the final (S)LDC constraint.
  | StartEnd Int      -- ^ @StartEnd@ indicates where lookup end selectors begin.     
  deriving (Eq,Ord,Show)

lookupSelectorIndex :: LookupSelector -> Int
lookupSelectorIndex sel = case sel of
  TransSre   -> 0
  TransLdc   -> 1
  InitSre    -> 2
  LastLdc    -> 3
  StartEnd k -> 4 + k

--------------------------------------------------------------------------------

evalLookupEquations :: CommonCircuitData -> [FExt] -> OpeningSet -> ProofChallenges -> [FExt]
evalLookupEquations (MkCommonCircuitData{..}) lkpSels openings challenges = final where

  MkCircuitConfig  {..} = circuit_config
  MkOpeningSet     {..} = openings
  MkProofChallenges{..} = challenges 

  nluts          = length circuit_luts
  num_challenges = config_num_challenges

  selector :: LookupSelector -> FExt
  selector sel = lkpSels !! (lookupSelectorIndex sel)

  -- one for each challenge round
  roundChunks :: [[(FExt,FExt)]]
  roundChunks = partition circuit_num_lookup_polys $ zip opening_lookup_zs opening_lookup_zs_next
  
  final = concat $ safeZipWith roundWorker plonk_deltas roundChunks

  num_lu_slots   = config_num_routed_wires `Prelude.div` 2     -- lookups per LookupGate
  num_lut_slots  = config_num_routed_wires `Prelude.div` 3     -- entries per LookupTableGate
  num_sldc_polys = circuit_num_lookup_polys - 1
  lu_degree      = circuit_quotient_degree_factor - 1 
  lut_degree     = num_lut_slots `divCeil` num_sldc_polys

  -- computation for a single challenge round
  roundWorker :: LookupDelta -> [(FExt,FExt)] -> [FExt]
  roundWorker (MkLookupDelta{..}) columns = final where

    (re_pair:sldc_pairs) = columns
    (re  ,re_next  ) = re_pair
    (sldc,sldc_next) = unzip sldc_pairs

    lu_combos    = [ (inp + scaleExt lookup_A out) | [inp,out]      <- take num_lu_slots  (partition 2 opening_wires) ] :: [FExt]
    lut_combos_A = [ (inp + scaleExt lookup_A out) | [inp,out,mult] <- take num_lut_slots (partition 3 opening_wires) ] :: [FExt]
    lut_combos_B = [ (inp + scaleExt lookup_B out) | [inp,out,mult] <- take num_lut_slots (partition 3 opening_wires) ] :: [FExt]

    mult i = opening_wires !! (3*i+2)
    mults  = [ mult i | i<-[0..num_lut_slots-1] ]

    chunks_lu_combo  = partition lu_degree  lu_combos
    chunks_lut_combo = partition lut_degree lut_combos_A
    chunks_mults     = partition lut_degree mults

    final  = [ eq_last_sldc , eq_ini_sum , eq_ini_re ]
          ++ eq_finals_re
          ++ [ eq_re_trans ]
          ++ eqs_sldc

    eq_last_sldc = selector LastLdc * (last sldc)         -- SLDC sums to zero
    eq_ini_sum   = selector InitSre * (head sldc)         -- SUM starts with zero
    eq_ini_re    = selector InitSre * re                  -- RE starts with zero

    -- RE ends are correct (for each LUT)
    eq_finals_re = 
      [ selector (StartEnd k) * (re - fromBase (evalFinalRE table)) 
      | (k,table) <- zip [0..] circuit_luts 
      ] 
    evalFinalRE table = cur_eval where
      lut         = fromLookupTable table
      lut_nrows   = length lut `divCeil` num_lut_slots               -- number of rows in this table
      padded_size = lut_nrows * num_lut_slots                        -- padded size of the table
      lut_padded  = take padded_size $ lut ++ repeat (head lut)      -- NOTE: the padding was fixed in commit 091047f
      cur_eval    = foldl' (\acc x -> lookup_delta * acc + x) 0 
                      [ (inp + lookup_B * out) | (inp,out) <- lut_padded ]

    -- RE transition is correct
    eq_re_trans = selector TransSre * (re - cur_sum) where
      cur_sum = foldl' (\acc elt -> scaleExt lookup_delta acc + elt) re_next lut_combos_B 

    prevThisPairs = pairs (last sldc_next : sldc)
    eqs_sldc = concatMap evalTransSLDC $ zip prevThisPairs (zip3 chunks_lu_combo chunks_lut_combo chunks_mults)

    -- LDC and SUM transitions are correct
    evalTransSLDC ((prev,this),(lu_combos,lut_combos,mults)) = [ eq_sum_trans , eq_ldc_trans ] where

      alpha = fromBase lookup_alpha :: FExt

      lu_prod     = product [ alpha - combo | combo <- lu_combos  ] :: FExt
      lut_prod    = product [ alpha - combo | combo <- lut_combos ] :: FExt
      lu_prods_i  = [        product [ alpha - combo | combo <- one_less ] |       one_less  <-            remove1 lu_combos   ] :: [FExt]
      lut_prods_i = [ mult * product [ alpha - combo | combo <- one_less ] | (mult,one_less) <- zip mults (remove1 lut_combos) ] :: [FExt]

      lu_sum_prod  = sum lu_prods_i
      lut_sum_prod = sum lut_prods_i

      eq_ldc_trans = selector TransLdc * (  lu_prod * (this - prev) + lu_sum_prod  )
      eq_sum_trans = selector TransSre * ( lut_prod * (this - prev) - lut_sum_prod )

--------------------------------------------------------------------------------
