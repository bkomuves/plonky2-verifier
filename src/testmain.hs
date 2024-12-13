
module Main where

--------------------------------------------------------------------------------

import Data.Aeson

import Types
import Hash
import Digest
import Algebra.Goldilocks
import Challenge.Verifier

import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L

--------------------------------------------------------------------------------

main = do
  -- let prefix = "fibonacci"
  -- let prefix = "recursion_outer"
  let prefix = "lookup"

  text_common <- L.readFile ("../json/" ++ prefix ++ "_common.json")
  text_vkey   <- L.readFile ("../json/" ++ prefix ++ "_vkey.json"  )
  text_proof  <- L.readFile ("../json/" ++ prefix ++ "_proof.json" )

  let Just common_data   = decode text_common :: Maybe CommonCircuitData
  let Just verifier_data = decode text_vkey   :: Maybe VerifierOnlyCircuitData
  let Just proof_data    = decode text_proof  :: Maybe ProofWithPublicInputs

  let challenges = proofChallenges common_data verifier_data proof_data

  print challenges