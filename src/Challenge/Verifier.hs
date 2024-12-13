
-- | The verifier's challenges 

{-# LANGUAGE StrictData #-}
module Challenge.Verifier where

--------------------------------------------------------------------------------

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Hash
import Digest
import Types

import Challenge.Monad
import Challenge.FRI

--------------------------------------------------------------------------------

-- | Lookup challenges (called \"deltas\" in Plonky2)
data LookupDelta = MkLookupDelta
  { lookup_A     :: F                -- ^ used to combine the lookup input and output in the argument
  , lookup_B     :: F                -- ^ used to combine the lookup input and output in the LUT consistency check
  , lookup_alpha :: F                -- ^ the random value in the log-derivative lookup argument
  , lookup_delta :: F                -- ^ the point to evaluate the lookup polynomial on (for the consistency check?)
  } 
  deriving (Eq,Show)

mkLookupDelta :: [F] -> LookupDelta
mkLookupDelta [a,b,c,d] = MkLookupDelta a b c d
mkLookupDelta _         = error "mkLookupDelta: expecting 4 field elements"

-- | Plonky2 does this rather strangely... First it reuses the plonk betas and gammas,
-- then extend with the required number of new challenges, then partition it
-- into groups of four challenges
mkLookupDeltaList :: [F] -> [LookupDelta]
mkLookupDeltaList = go where
  go [] = []
  go fs = case splitAt 4 fs of 
    (this,rest) -> mkLookupDelta this : go rest 

--------------------------------------------------------------------------------

-- | All challenges
data ProofChallenges = MkProofChallenges
  { plonk_betas    :: [F]            -- ^ Random values used in Plonk's permutation argument.
  , plonk_gammas   :: [F]            -- ^ Random values used in Plonk's permutation argument.
  , plonk_alphas   :: [F]            -- ^ Random values used to combine PLONK constraints.
  , plonk_deltas   :: [LookupDelta]  -- ^ Lookup challenges.
  , plonk_zeta     :: FExt           -- ^ Point at which the PLONK polynomials are opened.
  , fri_challenges :: FriChallenges
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

-- | Derive all challenges
proofChallenges :: CommonCircuitData -> VerifierOnlyCircuitData -> ProofWithPublicInputs -> ProofChallenges
proofChallenges common_data verifier_data proof_data = runDuplex action zeroState where

  MkVerifierOnlyCircuitData constants_sigmas_cap circuit_digest = verifier_data
  MkProofWithPublicInputs   proof public_io                     = proof_data

  config         = circuit_config common_data
  num_challenges = config_num_challenges config
  has_lookup     = circuit_num_lookup_polys common_data > 0
  
  public_inputs_hash = sponge public_io

  action :: Duplex ProofChallenges
  action = do

    absorb circuit_digest
    absorb public_inputs_hash
    absorb (wires_cap proof)

    -- plonk challenges
    plonk_betas  <- squeezeN num_challenges :: Duplex [F]
    plonk_gammas <- squeezeN num_challenges :: Duplex [F]

    -- lookup challenges
    plonk_deltas <- case has_lookup of
      False -> return []
      True  -> do
        deltas <- squeezeN (2*num_challenges) 
        return $ mkLookupDeltaList (plonk_betas ++ plonk_gammas ++ deltas)

    absorb $ plonk_zs_partial_products_cap proof
    plonk_alphas <- squeezeN num_challenges :: Duplex [F]

    absorb $ quotient_polys_cap proof
    plonk_zeta <- squeeze :: Duplex FExt

    fri_challenges <- friChallenges common_data verifier_data proof

    return $ MkProofChallenges
      { plonk_betas    = plonk_betas   
      , plonk_gammas   = plonk_gammas  
      , plonk_alphas   = plonk_alphas   
      , plonk_deltas   = plonk_deltas   
      , plonk_zeta     = plonk_zeta     
      , fri_challenges = fri_challenges 
      }

--------------------------------------------------------------------------------
