
-- | The FRI protocol's challenges 

{-# LANGUAGE StrictData, RecordWildCards, NondecreasingIndentation #-}
module Challenge.FRI where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Bits

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Hash.Sponge
import Hash.Digest
import Challenge.Monad
import Types
import Misc.Aux

--------------------------------------------------------------------------------

-- | FRI protocol challenges
data FriChallenges = MkFriChallenges 
  { fri_alpha         :: FExt        -- ^ Scaling factor to combine polynomials.
  , fri_betas         :: [FExt]      -- ^ Betas used in the FRI commit phase reductions.
  , fri_pow_response  :: F           -- ^ proof-of-work \"response\"
  , fri_query_indices :: [Int]       -- ^ Indices at which the oracle is queried in FRI.
  }
  deriving (Eq,Show)

--------------------------------------------------------------------------------

newtype FriOpenings 
  = MkFriOpenings { batches :: [FriOpeningBatch] }
  deriving (Eq,Show)

newtype FriOpeningBatch
  = MkFriOpeningBatch { values :: [FExt] }
  deriving (Eq,Show)

absorbFriOpenings :: FriOpenings -> Duplex ()
absorbFriOpenings (MkFriOpenings batches) = mapM_ (absorb . values) batches

-- | Just /reordering/ and concatenating things...
toFriOpenings :: OpeningSet -> FriOpenings 
toFriOpenings (MkOpeningSet{..}) = MkFriOpenings [ batch_this, batch_next ] 
  where
    batch_this = MkFriOpeningBatch $ concat 
      [ opening_constants        
      , opening_plonk_sigmas     
      , opening_wires            
      , opening_plonk_zs         
      , opening_partial_products 
      , opening_quotient_polys   
      , opening_lookup_zs        
      ]
    batch_next = MkFriOpeningBatch $ concat
      [ opening_plonk_zs_next
      , opening_lookup_zs_next
      ]

--------------------------------------------------------------------------------

friChallenges ::  CommonCircuitData -> VerifierOnlyCircuitData -> Proof -> Duplex FriChallenges
friChallenges common_data verifier_data proof = do
  let config      = circuit_config common_data
  let fri_config  = config_fri_config config  
  let openings    = Types.openings      proof
  let fri_proof   = Types.opening_proof proof
  let degree_bits = fri_degree_bits (circuit_fri_params common_data)

  absorbFriOpenings $ toFriOpenings openings

  -- Scaling factor to combine polynomials.
  alpha <- squeeze :: Duplex FExt

  -- Recover the random betas used in the FRI reductions.
  betas <- forM (fri_commit_phase_merkle_caps fri_proof) $ \cap -> do
    absorb cap
    squeeze

  absorb $ coeffs $ fri_final_poly fri_proof

  -- proof of work or "grinding" 
  -- they are doing this in a quite strange way... 
  -- namely: absorb the candidate prover witness, generate a Fiat-Shamir "challenge", 
  -- and check the leading zeros of _that_. Let's say I would do it differently
  absorb $ fri_pow_witness fri_proof
  pow_response <- squeeze

   -- query indices
  let lde_size    = exp2' (degree_bits + fri_rate_bits fri_config)
  let num_fri_queries = fri_num_query_rounds fri_config
  let f :: F -> Int  
      f felt = fromInteger (mod (asInteger felt) lde_size)
  query_indices <- (map f) <$> (squeezeN num_fri_queries)

  return $  MkFriChallenges 
    { fri_alpha         = alpha
    , fri_betas         = betas
    , fri_pow_response  = pow_response
    , fri_query_indices = query_indices
    }

--------------------------------------------------------------------------------


