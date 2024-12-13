
{-# LANGUAGE StrictData, OverloadedStrings, DeriveGeneric, DeriveAnyClass, NoGeneralizedNewtypeDeriving #-}
module Types where

--------------------------------------------------------------------------------

import Data.Char
import Data.Word

import Data.Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types ( Result(..) )

import qualified Data.ByteString.Lazy.Char8 as L

import GHC.Generics

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Hash.Digest
import Gate.Base
import Gate.Parser

--------------------------------------------------------------------------------

newtype LookupTable 
  = MkLookupTable [(Word64,Word64)]
  deriving (Eq,Show,Generic)

instance ToJSON   LookupTable where toJSON (MkLookupTable x) = toJSON x
instance FromJSON LookupTable where parseJSON o = MkLookupTable <$> parseJSON o

newtype PolynomialCoeffs coeff
  = MkPolynomialCoeffs { coeffs :: [coeff] }
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

--------------------------------------------------------------------------------
-- * Common circuit data

data CommonCircuitData = MkCommonCircuitData
  { circuit_config                 :: CircuitConfig    -- ^ Global circuit configuration
  , circuit_fri_params             :: FriParams        -- ^ FRI parameters
  , circuit_gates                  :: [Gate]           -- ^ The types of gates used in this circuit, along with their prefixes.
  , circuit_selectors_info         :: SelectorsInfo    -- ^ Information on the circuit's selector polynomials.
  , circuit_quotient_degree_factor :: Int              -- ^ The degree of the PLONK quotient polynomial.
  , circuit_num_gate_constraints   :: Int              -- ^ The largest number of constraints imposed by any gate.
  , circuit_num_constants          :: Int              -- ^ The number of constant wires.
  , circuit_num_public_inputs      :: Int              -- ^ Number of public inputs
  , circuit_k_is                   :: [F]              -- ^ The @{k_i}@ values (coset shifts) used in @S_I D_i@ in Plonk's permutation argument.
  , circuit_num_partial_products   :: Int              -- ^ The number of partial products needed to compute the `Z` polynomials.
  , circuit_num_lookup_polys       :: Int              -- ^ The number of lookup polynomials.
  , circuit_num_lookup_selectors   :: Int              -- ^ The number of lookup selectors.
  , circuit_luts                   :: [LookupTable]    -- ^ The stored lookup tables.
  }
  deriving (Eq,Show,Generic)

instance FromJSON CommonCircuitData where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }
--instance ToJSON   CommonCircuitData where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 8 }

data CircuitConfig = MkCircuitConfig
  { config_num_wires                  :: Int        -- ^ Number of wires available at each row. This corresponds to the "width" of the circuit, and consists in the sum of routed wires and advice wires.
  , config_num_routed_wires           :: Int        -- ^ The number of routed wires, i.e. wires that will be involved in Plonk's permutation argument.
  , config_num_constants              :: Int        -- ^ The number of constants that can be used per gate.
  , config_use_base_arithmetic_gate   :: Bool       -- ^ Whether to use a dedicated gate for base field arithmetic, rather than using a single gate for both base field and extension field arithmetic.
  , config_security_bits              :: Int        -- ^ Security level target
  , config_num_challenges             :: Int        -- ^ The number of challenge points to generate, for IOPs that have soundness errors of (roughly) `degree / |F|`.
  , config_zero_knowledge             :: Bool       -- ^ Option to activate the zero-knowledge property.
  , config_randomize_unused_wires     :: Bool       -- ^ Option to disable randomization (useful for debugging).
  , config_max_quotient_degree_factor :: Int        -- ^ A cap on the quotient polynomial's degree factor.
  , config_fri_config                 :: FriConfig
  }
  deriving (Eq,Show,Generic)

instance FromJSON CircuitConfig where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }
instance ToJSON   CircuitConfig where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 7 }

-- | The interval @[a,b)@ (inclusive on the left, exclusive on the right)
data Range = MkRange 
  { range_start :: Int
  , range_end   :: Int
  }
  deriving (Eq,Show,Generic)

instance FromJSON Range where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }
instance ToJSON   Range where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 6 }

data SelectorsInfo = MkSelectorsInfo
  { selector_indices :: [Int]             -- ^ which gate is in which selector groups (length = number of gates)
  , selector_groups  :: [Range]           -- ^ the selector groups are continuous intervals
  , selector_vector  :: Maybe [Int]       -- ^ this is an unofficial addition, so it's optional
  }
  deriving (Eq,Show,Generic)

instance FromJSON SelectorsInfo where
  parseJSON = withObject "SelectorsInfo" $ \v -> MkSelectorsInfo
    <$> v .:  "selector_indices"
    <*> v .:  "groups"
    <*> v .:? "selector_vector"

instance ToJSON SelectorsInfo where
  toJSON selInfo = object (mandatory ++ optional) where
    mandatory = 
      [ "selector_indices" .= toJSON (selector_indices selInfo)
      , "groups"           .= toJSON (selector_groups  selInfo)
      ]
    optional = case selector_vector selInfo of 
      Nothing     -> [] 
      Just selvec -> [ "selector_vector" .= toJSON selvec ]

--------------------------------------------------------------------------------
-- * FRI types

data FriConfig = MkFrConfig 
  { fri_rate_bits          :: Int                      -- ^ @rate = 2^{-rate_bits}@
  , fri_cap_height         :: Int                      -- ^ Height of Merkle tree caps.
  , fri_proof_of_work_bits :: Int                      -- ^ Number of bits used for grinding.
  , fri_reduction_strategy :: FriReductionStrategy     -- ^ The reduction strategy to be applied at each layer during the commit phase.
  , fri_num_query_rounds   :: Int                      -- ^ Number of query rounds to perform.
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriConfig where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriConfig where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

data FriReductionStrategy 
  = Fixed             { arity_bits_seq :: [Int] }
  | ConstantArityBits { arity_bits :: Int , final_poly_bits :: Int }
  | MinSize           { opt_max_arity_bits :: Maybe Int }
  deriving (Eq,Show,Generic)

instance FromJSON FriReductionStrategy where
  parseJSON val = case val of
    Object obj -> case KeyMap.toList obj of
      [(key,val)] -> case key of
        "Fixed"             -> Fixed <$> parseJSON val
        "ConstantArityBits" -> (\[a,b] -> ConstantArityBits a b) <$> parseJSON val
        "MinSize"           -> MinSize <$> parseJSON val      -- TODO: this probably won't work because Maybe vs Option
        _ -> fail $ "FromJSON/FriReductionStrategy: unrecognized FRI reduction strategy: `" ++ show key ++ "`"
      _ -> fail "FromJSON/FriReductionStrategy: expecting a singleton object"
    _ -> fail "FromJSON/FriReductionStrategy: expecting an object"

instance ToJSON FriReductionStrategy where
  toJSON strat = case strat of
    Fixed             xs  -> object [ "Fixed"             .= toJSON xs    ]
    ConstantArityBits x y -> object [ "ConstantArityBits" .= toJSON [x,y] ]
    MinSize           mb  -> error "ToJSON/FriReductionStrategy/MinSize: this is not handled yet"

data FriParams = MkFriParams
  { fri_config               :: FriConfig   -- ^ User-specified FRI configuration.
  , fri_hiding               :: Bool        -- ^ Whether to use a hiding variant of Merkle trees (where random salts are added to leaves).
  , fri_degree_bits          :: Int         -- ^ The degree of the purported codeword, measured in bits.
  , fri_reduction_arity_bits :: [Int]       -- ^ The arity of each FRI reduction step, expressed as the log2 of the actual arity.
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriParams where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriParams where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

data FriProof = MkFriProof
  { fri_commit_phase_merkle_caps :: [MerkleCap]               -- ^ A Merkle cap for each reduced polynomial in the commit phase.
  , fri_query_round_proofs       :: [FriQueryRound]           -- ^ Query rounds proofs
  , fri_final_poly               :: PolynomialCoeffs FExt     -- ^ The final polynomial in coefficient form.
  , fri_pow_witness              :: F                         -- ^ Witness showing that the prover did PoW.
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriProof where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriProof where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

data FriQueryRound = MkFriQueryRound 
  { fri_initial_trees_proof :: FriInitialTreeProof
  , fri_steps               :: [FriQueryStep]
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriQueryRound where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriQueryRound where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

newtype FriInitialTreeProof
  = MkFriInitialTreeProof { evals_proofs :: [ ( [F] , MerkleProof ) ] }
  deriving (Eq,Show,Generic,FromJSON,ToJSON)

data FriQueryStep = MkFriQueryStep
  { fri_evals        :: [FExt] 
  , fri_merkle_proof :: MerkleProof
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriQueryStep where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriQueryStep where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

newtype MerkleProof
  = MkMerkleProof { siblings :: [Digest] }
  deriving (Eq,Show,Generic,FromJSON,ToJSON)

--------------------------------------------------------------------------------
-- * Verifier

newtype PublicInputs 
  = MkPublicInputs [F]
  deriving (Eq,Show,Generic)

data VerifierCircuitData = MkVerifierCircuitData
  { verifier_only   :: VerifierOnlyCircuitData
  , verifier_common :: CommonCircuitData
  }
  deriving (Eq,Show,Generic)

newtype MerkleCap 
  = MkMerkleCap [Digest]
  deriving (Eq,Show,Generic)

instance ToJSON   MerkleCap where toJSON (MkMerkleCap caps) = toJSON caps
instance FromJSON MerkleCap where parseJSON o = MkMerkleCap <$> parseJSON o

data VerifierOnlyCircuitData = MkVerifierOnlyCircuitData
  { constants_sigmas_cap :: MerkleCap     -- ^ commitment to list of constant polynomial and sigma polynomials
  , circuit_digest       :: Digest        -- ^ a digest of the "circuit" (i.e. the instance, minus public inputs), which can be used to seed Fiat-Shamir
  }
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

--------------------------------------------------------------------------------
-- * Proof

data ProofWithPublicInputs = MkProofWithPublicInputs
  { the_proof     :: Proof
  , public_inputs :: [F]
  }
  deriving (Eq,Show,Generic)

instance FromJSON ProofWithPublicInputs where
  parseJSON = withObject "ProofWithPublicInputs" $ \v -> MkProofWithPublicInputs
    <$> v .:  "proof"
    <*> v .:  "public_inputs"

data Proof = MkProof 
  { wires_cap          :: MerkleCap               -- ^ Merkle cap of LDEs of wire values.
  , plonk_zs_partial_products_cap :: MerkleCap    -- ^ Merkle cap of LDEs of Z, in the context of Plonk's permutation argument.
  , quotient_polys_cap :: MerkleCap               -- ^ Merkle cap of LDEs of the quotient polynomial components.
  , openings           :: OpeningSet              -- ^ Purported values of each polynomial at the challenge point.
  , opening_proof      :: FriProof                -- ^ A batch FRI argument for all openings.
  }
  deriving (Eq,Show,Generic,ToJSON,FromJSON)

data OpeningSet = MkOpeningSet
  { opening_constants        :: [FExt]
  , opening_plonk_sigmas     :: [FExt]
  , opening_wires            :: [FExt]
  , opening_plonk_zs         :: [FExt]
  , opening_plonk_zs_next    :: [FExt]
  , opening_partial_products :: [FExt]
  , opening_quotient_polys   :: [FExt]
  , opening_lookup_zs        :: [FExt]
  , opening_lookup_zs_next   :: [FExt]
  }
  deriving (Eq,Show,Generic)

instance FromJSON OpeningSet where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }
instance ToJSON   OpeningSet where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 8 }

--------------------------------------------------------------------------------

-- seriously...
decodeString :: FromJSON a => String -> Maybe a
decodeString = decode . L.pack

encodeString :: ToJSON a => a -> String
encodeString = L.unpack . encode

--------------------------------------------------------------------------------
