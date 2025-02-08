
{-# LANGUAGE StrictData, OverloadedStrings, DeriveGeneric, DeriveAnyClass, NoGeneralizedNewtypeDeriving #-}
module Types where

--------------------------------------------------------------------------------

import Data.Char
import Data.Word
import Data.List

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
import Misc.Aux

--------------------------------------------------------------------------------

newtype LookupTable 
  = MkLookupTable [(Word64,Word64)]
  deriving (Eq,Show,Generic)

fromLookupTable :: LookupTable -> [(F,F)] 
fromLookupTable (MkLookupTable pairs) = [ (toF inp, toF out) | (inp,out) <- pairs ]

instance ToJSON   LookupTable where toJSON (MkLookupTable x) = toJSON x
instance FromJSON LookupTable where parseJSON o = MkLookupTable <$> parseJSON o

----------------------------------------

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
  , circuit_num_constants          :: Int              -- ^ The number of constant columns wires.
  , circuit_num_public_inputs      :: Int              -- ^ Number of public inputs
  , circuit_k_is                   :: [F]              -- ^ The @{k_i}@ values (coset shifts) used in @S_I D_i@ in Plonk's permutation argument.
  , circuit_num_partial_products   :: Int              -- ^ The number of partial products needed to compute the `Z` polynomials; @ = ceil( #routed / max_degree ) - 1@
  , circuit_num_lookup_polys       :: Int              -- ^ The number of lookup polynomials.
  , circuit_num_lookup_selectors   :: Int              -- ^ The number of lookup selectors.
  , circuit_luts                   :: [LookupTable]    -- ^ The stored lookup tables.
  }
  deriving (Eq,Show,Generic)

circuit_nrows :: CommonCircuitData -> Int
circuit_nrows = fri_nrows . circuit_fri_params

circuit_num_luts :: CommonCircuitData -> Int
circuit_num_luts = length . circuit_luts

instance FromJSON CommonCircuitData where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 8 }
--instance ToJSON   CommonCircuitData where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 8 }

data CircuitConfig = MkCircuitConfig
  { config_num_wires                  :: Int        -- ^ Number of wires available at each row. This corresponds to the "width" of the circuit, and consists in the sum of routed wires and advice wires.
  , config_num_routed_wires           :: Int        -- ^ The number of routed wires, i.e. wires that will be involved in Plonk's permutation argument.
  , config_num_constants              :: Int        -- ^ The number of constants that can be used per gate.
  , config_use_base_arithmetic_gate   :: Bool       -- ^ Whether to use a dedicated gate for base field arithmetic, rather than using a single gate for both base field and extension field arithmetic.
  , config_security_bits              :: Log2       -- ^ Security level target
  , config_num_challenges             :: Int        -- ^ The number of challenge points to generate, for IOPs that have soundness errors of (roughly) `degree / |F|`.
  , config_zero_knowledge             :: Bool       -- ^ Option to activate the zero-knowledge property.
  , config_randomize_unused_wires     :: Bool       -- ^ Option to disable randomization (useful for debugging).
  , config_max_quotient_degree_factor :: Int        -- ^ A cap on the quotient polynomial's degree factor.
  , config_fri_config                 :: FriConfig
  }
  deriving (Eq,Show,Generic)

instance FromJSON CircuitConfig where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }
instance ToJSON   CircuitConfig where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 7 }

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

data FriConfig = MkFriConfig 
  { fri_rate_bits          :: Log2                     -- ^ @rate = 2^{-rate_bits}@
  , fri_cap_height         :: Log2                     -- ^ Height of Merkle tree caps.
  , fri_proof_of_work_bits :: Log2                     -- ^ Number of bits used for grinding.
  , fri_reduction_strategy :: FriReductionStrategy     -- ^ The reduction strategy to be applied at each layer during the commit phase.
  , fri_num_query_rounds   :: Int                      -- ^ Number of query rounds to perform.
  }
  deriving (Eq,Show,Generic)

instance FromJSON FriConfig where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }
instance ToJSON   FriConfig where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 4 }

data FriReductionStrategy 
  = Fixed             { arity_bits_seq :: [Log2] }
  | ConstantArityBits { arity_bits :: Log2 , final_poly_bits :: Log2 }
  | MinSize           { opt_max_arity_bits :: Maybe Log2 }
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
  , fri_degree_bits          :: Log2        -- ^ The degree of the purported codeword, measured in bits.
  , fri_reduction_arity_bits :: [Log2]      -- ^ The arity of each FRI reduction step, expressed as the log2 of the actual arity.
  }
  deriving (Eq,Show,Generic)

-- | Number of rows in the circuit
fri_nrows :: FriParams -> Int
fri_nrows params = exp2 nbits where
  nbits = fri_degree_bits params

-- | Logarithm of the size of the LDE codeword
fri_LDE_bits :: FriParams -> Log2
fri_LDE_bits params = nbits where
  nbits = fri_degree_bits params + fri_rate_bits (fri_config params)

-- | Number of rows in the LDE codewords
fri_LDE_nrows :: FriParams -> Int
fri_LDE_nrows params = exp2 (fri_LDE_bits params)

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

merkleCapSize :: MerkleCap -> Int
merkleCapSize (MkMerkleCap ds) = length ds

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
  { opening_constants        :: [FExt]         -- ^ note: this includes the selector columns!
  , opening_plonk_sigmas     :: [FExt]         -- ^ these correspond to columns encoding the wire permutation (as many as routed columns)
  , opening_wires            :: [FExt]         -- ^ these are evaluations of the witness columns
  , opening_plonk_zs         :: [FExt]         -- ^ first columns of the permutation arguments (there @num_challenges@ number of these)
  , opening_plonk_zs_next    :: [FExt]         -- ^ evaluations of the first columns shifted by \"one row\"
  , opening_partial_products :: [FExt]         -- ^ remaining columns of the permutation arguments
  , opening_quotient_polys   :: [FExt]         -- ^ the quotient polynomials (@num_challenges * max_degree@)
  , opening_lookup_zs        :: [FExt]         -- ^ columns of the lookup arguments
  , opening_lookup_zs_next   :: [FExt]         -- ^ evaluation of the lookup columns evaluated shifted by \"one row\"
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
