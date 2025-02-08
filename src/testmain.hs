
{-# LANGUAGE RecordWildCards #-}
module Main where

--------------------------------------------------------------------------------

import Data.Aeson

import Types
import Hash.Sponge
import Hash.Digest
import Algebra.Goldilocks
import Challenge.Verifier
import Plonk.Verifier

import Plonk.Vanishing
import Plonk.FRI

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------------------------

main = do
  -- let prefix = "fibonacci"
  -- let prefix = "lookup"
  -- let prefix = "multi_lookup"
  -- let prefix = "recursion_middle"
  let prefix = "recursion_outer"

  text_common <- L.readFile ("../json/" ++ prefix ++ "_common.json")
  text_vkey   <- L.readFile ("../json/" ++ prefix ++ "_vkey.json"  )
  text_proof  <- L.readFile ("../json/" ++ prefix ++ "_proof.json" )

  let Just common_data   = decode text_common :: Maybe CommonCircuitData
  let Just verifier_data = decode text_vkey   :: Maybe VerifierOnlyCircuitData
  let Just proof_data    = decode text_proof  :: Maybe ProofWithPublicInputs
  let vkey = MkVerifierCircuitData verifier_data common_data

  let pi_hash = sponge (public_inputs proof_data)
  putStrLn $ "public inputs hash = " ++ show pi_hash

  let MkOpeningSet{..} = openings (the_proof proof_data)
  putStrLn $ "# opening_constants        = " ++ show (length opening_constants       ) 
  putStrLn $ "# opening_plonk_sigmas     = " ++ show (length opening_plonk_sigmas    ) 
  putStrLn $ "# opening_wires            = " ++ show (length opening_wires           ) 
  putStrLn $ "# opening_plonk_zs         = " ++ show (length opening_plonk_zs        ) 
  putStrLn $ "# opening_plonk_zs_next    = " ++ show (length opening_plonk_zs_next   ) 
  putStrLn $ "# opening_partial_products = " ++ show (length opening_partial_products) 
  putStrLn $ "# opening_quotient_polys   = " ++ show (length opening_quotient_polys  ) 
  putStrLn $ "# opening_lookup_zs        = " ++ show (length opening_lookup_zs       ) 
  putStrLn $ "# opening_lookup_zs_next   = " ++ show (length opening_lookup_zs_next  ) 

  let challenges = proofChallenges common_data verifier_data proof_data

  -- print challenges

  print $ evalCombinedPlonkConstraints common_data proof_data challenges
  print $ checkCombinedPlonkEquations' common_data proof_data challenges

  --  debugFRI common_data verifier_data (the_proof proof_data) challenges

  putStrLn $ "proof verification result = " ++ show (verifyProof vkey proof_data)
