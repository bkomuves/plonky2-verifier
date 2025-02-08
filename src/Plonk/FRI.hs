
-- | Verify the FRI protocol

{-# LANGUAGE BangPatterns, StrictData, RecordWildCards, DeriveFunctor, DeriveFoldable #-}
module Plonk.FRI where

--------------------------------------------------------------------------------

import Data.Array
import Data.Bits
import Data.Word
import Data.List
import Data.Foldable

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.FFT

import Challenge.FRI
import Challenge.Verifier

import Hash.Digest
import Hash.Sponge
import Hash.Merkle

import Types
import Misc.Aux

{-
-- debugging only
import Text.Printf    
import Challenge.Verifier
import Debug.Trace
debug !msg !x y = trace (">>> " ++ msg ++ ": " ++ show x) y
-}

--------------------------------------------------------------------------------

data Oracles a = MkOracles 
  { oracle_constants :: a
  , oracle_witness   :: a
  , oracle_pp_lookup :: a
  , oracle_quotient  :: a
  }
  deriving (Show,Functor,Foldable)

enumerateOracles :: Oracles a -> [a]
enumerateOracles (MkOracles{..}) = 
  [ oracle_constants
  , oracle_witness
  , oracle_pp_lookup
  , oracle_quotient
  ]

-- | Size of the 4 oracle matrices
oracleWidths :: CommonCircuitData -> Oracles Int
oracleWidths (MkCommonCircuitData{..}) = widths where
  MkCircuitConfig{..} = circuit_config
  widths = MkOracles
    { oracle_constants = circuit_num_constants + config_num_routed_wires
    , oracle_witness   = config_num_wires
    , oracle_pp_lookup = r * (1 + circuit_num_partial_products + circuit_num_lookup_polys)
    , oracle_quotient  = r * circuit_quotient_degree_factor 
    }
  r = config_num_challenges

buildListOracle :: Oracles Int -> [[a]] -> Oracles [a]
buildListOracle (MkOracles lc lw lp lq) [c,w,p,q]
  = if (length c == lc) &&
       (length w == lw) &&
       (length p == lp) &&
       (length q == lq) 
    then MkOracles c w p q
    else error "buildListOracle: list size do not match the expected"
buildListOracle _ _ = error "buildListOracle: expecting a list of 4 lists"

----------------------------------------

validateMerkleCapLength :: Log2 -> MerkleCap -> MerkleCap
validateMerkleCapLength height cap@(MkMerkleCap roots) 
  | ok         = cap
  | otherwise  = error "validateMerkleCapLength: cap has wrong size"
  where
    ok  = length roots == len
    len = exp2 height

toMerkleOracles :: VerifierCircuitData -> Proof -> Oracles MerkleCap
toMerkleOracles (MkVerifierCircuitData{..}) (MkProof{..}) = oracles where
  MkCommonCircuitData{..} = verifier_common
  MkFriParams{..}         = circuit_fri_params 
  validate = validateMerkleCapLength (fri_cap_height fri_config) 
  oracles = MkOracles 
    { oracle_constants = validate $ constants_sigmas_cap verifier_only
    , oracle_witness   = validate $ wires_cap 
    , oracle_pp_lookup = validate $ plonk_zs_partial_products_cap
    , oracle_quotient  = validate $ quotient_polys_cap
    }

--------------------------------------------------------------------------------
-- * Initial tree proofs

-- | Checks the initial tree proofs, and returns
-- evaluation oracles at @x = g * (eta ^ query_idx)@
-- (it's just some rearrangement...)
checkInitialTreeProofs :: CommonCircuitData -> Oracles MerkleCap -> FExt -> Int -> FriInitialTreeProof -> Oracles [F]
checkInitialTreeProofs common_data oracles alpha query_idx (MkFriInitialTreeProof{..})
  | length evals_proofs /= 4  = error "checkInitialTreeProofs: expecting 4 Merkle proofs for the 4 oracles"
  | not merkle_are_ok         = error "checkInitialTreeProofs: at least one Merkle proof failed"
  | otherwise                 = result
  where
    merkle_are_ok = and 
      [ checkMerkleProof cap query_idx leaf proof 
      | (cap,(leaf,proof)) <- safeZip (enumerateOracles oracles) evals_proofs
      ]
    config = circuit_config common_data
    widths = oracleWidths common_data
    result = buildListOracle widths (map fst evals_proofs)

-- | Combinations (with powers of alpha) of openings
data PrecomputedReducedOpenings = MkPrecomputedReducedOpenings
  { sum_this_row :: FExt  -- ^ sum over the openings of the full rows
  , sum_next_row :: FExt  -- ^ sum over the few openings we need from the \"next row\"
  }
  deriving Show

----------------------------------------

precomputeReducedOpenings :: FExt -> FriOpenings -> PrecomputedReducedOpenings
precomputeReducedOpenings alpha (MkFriOpenings [one,two]) = result where
  result = MkPrecomputedReducedOpenings this next 
  this = reduceWithPowers alpha row1
  next = reduceWithPowers alpha row2
  MkFriOpeningBatch row1 = one
  MkFriOpeningBatch row2 = two

-- | Calculates the evaluation of the \"combined polynomial\" at @x0 = g * eta^query_idx@
--
-- More precisely, this is
--
-- >   G0(X) - Y0                      G1(X) - Y1
-- >  ------------  +   alpha^M * ------------------
-- >    X - zeta                    X - omega*zeta
--
-- where (Y0,Y1) are the \"precomputed reduced openings\",
-- G0(X) and G1(X) are the column polynomial "batches" combined by powers of @alpha@,
-- and M is the size of the first batch. Finally @X -> x0@ is substituted.
--
-- The first batch contains all columns, the second only
-- "zs" and "lookup_zs".
--
combineInitial :: CommonCircuitData -> ProofChallenges -> PrecomputedReducedOpenings -> Oracles [F] -> Int -> FExt
combineInitial (MkCommonCircuitData{..}) (MkProofChallenges{..}) preComp oracles@(MkOracles{..}) query_idx
  | sanityCheck   = result
  | otherwise     = error "combineInitial: sanity check failed"
  where

    MkCircuitConfig{..} = circuit_config
    MkFriChallenges{..} = fri_challenges

    MkPrecomputedReducedOpenings y0 y1 = preComp

    zeta  = plonk_zeta
    alpha = fri_alpha

    r   = config_num_challenges
    npp = divCeil config_num_routed_wires circuit_quotient_degree_factor

    sanityCheck = r * (npp + circuit_num_lookup_polys) == length oracle_pp_lookup

    (oracle_pp,oracle_lookup) = splitAt (r*npp) oracle_pp_lookup
  
    -- NOTE: this is /reordered/ the same way as FriOpenings,
    -- except that we don't have the same Openings input structure
    -- here to reuse...
    -- 
    -- the whole Plonky2 codebase is seriously full of WTF-ness
    firstBatch 
      =  oracle_constants
      ++ oracle_witness  
      ++ oracle_pp       
      ++ oracle_quotient 
      ++ oracle_lookup
    secondBatch 
      =  take r oracle_pp 
      ++ oracle_lookup

    len_1st_batch = length firstBatch
    len_2nd_batch = length secondBatch

    g0 = reduceWithPowers alpha (map fromBase firstBatch )
    g1 = reduceWithPowers alpha (map fromBase secondBatch)

    logn_small = fri_degree_bits circuit_fri_params 
    logn_lde   = fri_LDE_bits    circuit_fri_params 
    omega = subgroupGenerator logn_small
    eta   = subgroupGenerator logn_lde

    rev_idx = reverseBitsInt logn_lde query_idx
    point_x = fromBase (mulGen * pow_ eta rev_idx)

    loc0 = zeta
    loc1 = fromBase omega * zeta

    one = (g0 - y0) / (point_x - loc0)
    two = (g1 - y1) / (point_x - loc1)

    result = powExt_ alpha len_2nd_batch * one + two

--------------------------------------------------------------------------------
-- * Proof-of-work

checkProofOfWork :: FriConfig -> FriChallenges -> Bool
checkProofOfWork (MkFriConfig{..}) (MkFriChallenges{..}) = ok where
  lo_mask = fromInteger (exp2' fri_proof_of_work_bits - 1) :: Word64
  mask    = shiftL lo_mask (64 - fromLog2 fri_proof_of_work_bits)
  ok      = (fromF fri_pow_response .&. mask) == 0

--------------------------------------------------------------------------------
-- * Folding

-- | Note: query indices index into the bit-reversed-order arrays!!
data QueryIndex = MkQueryIndex 
  { query_array_size :: Log2
  , query_index_rev  :: Int
  }
  deriving (Eq,Ord,Show)

queryLocation :: F -> QueryIndex -> F
queryLocation shift (MkQueryIndex arr_size idx_rev) = loc where
  loc = shift * pow_ eta (reverseBitsInt arr_size idx_rev) 
  eta = subgroupGenerator arr_size

foldQueryIdx :: Log2 -> QueryIndex -> QueryIndex
foldQueryIdx arityLog2@(Log2 arity_bits) (MkQueryIndex oldSize oldIdx) = MkQueryIndex newSize newIdx where
  newSize = oldSize - arityLog2
  newIdx  = shiftR oldIdx arity_bits

-- | A coset of size @2^arity@, which is the unit we fold into a single field extension element
-- These are the leaves of the FRI commit phase Merkle trees
data Coset = MkCoset 
  { coset_size   :: Log2      -- ^ logarithm of the size of the coset
  , coset_offset :: F         -- ^ the coset is shifted from the subgroup by this element
  , coset_values :: [FExt]    -- ^ values of a polynomial on this coset
  }
  deriving Show

-- | Handling some of the fucked up conventions
prepareCoset :: F -> QueryIndex -> [FExt] -> Coset
prepareCoset shift (MkQueryIndex bigLog2 idx) values = coset where
  smallLog2@(Log2 arity) = safeLog2 (length values)
  ofs   = shift * pow_ eta start
  start = reverseBitsInt bigLog2 
        $ shiftL (shiftR idx arity) arity
  eta   = subgroupGenerator bigLog2
  coset = MkCoset 
    { coset_size   = smallLog2
    , coset_offset = ofs
    , coset_values = reverseIndexBitsList values
    }
 
-- | \"Folds\" a coset with a given folding coefficient @beta@
--
foldCosetWith :: FExt -> Coset -> FExt  -- (QueryIndex,Coset) -> (QueryIndex,FExt)
--foldCosetWith beta (oldQueryIdx,coset) = (newQueryIdx,final) where
foldCosetWith beta coset = final where 
  MkCoset arity_bits coset_x_loc xs = coset
  -- MkQueryIdx oldSize revIdx = oldQueryIdx
  -- newQueryIdx = foldQueryIdx arity_bits oldQueryIdx
  arity = exp2 arity_bits
  omega = subgroupGenerator arity_bits
  invArity = (1 :: F) / fromIntegral arity
  ys  = [ sum 
           [ scaleExt (pow_ x_omega_j (-k)) (xs!!j) 
           | j <- [0..arity-1] 
           , let x_omega_j = coset_x_loc * pow_ omega j
           ] 
        | k <- [0..arity-1] 
        ]
  final = scaleExt invArity $ sum $ zipWith (*) (powersOf beta) ys

data FoldingState = MkFoldingState
  { folding_shift         :: F
  , folding_query_idx     :: QueryIndex
  , folding_upstream_eval :: FExt
  }
  deriving Show

folding_query_loc :: FoldingState -> F
folding_query_loc (MkFoldingState shift (MkQueryIndex log2n idx) _eval) = loc where
  loc = shift * pow_ eta (reverseBitsInt log2n idx)
  eta = subgroupGenerator log2n

data FoldingInput = MkFoldingInput 
  { folding_arity       :: Log2
  , folding_beta        :: FExt
  , folding_merkle_cap  :: MerkleCap
  , folding_query_proof :: FriQueryStep
  }
  deriving Show

foldAll :: FoldingState -> [FoldingInput] -> FoldingState
foldAll = foldl' foldingStep 

-- | \"Folds\" a coset with a given folding coefficient @beta@
--
foldingStep :: FoldingState -> FoldingInput -> FoldingState
foldingStep 
  (MkFoldingState oldShift oldQueryIdx oldEval) 
  (MkFoldingInput arityLog2 beta merkleCap (MkFriQueryStep evals proof))
    | not proofCheckOK  = error "folding step Merkle proof does not check out"
    | not evalCheckOK   = error "folding step evaluation does not match the opening"
    | not arityCheckOK  = error "folding stpe: reduction strategy incompatibility"
    | otherwise         = MkFoldingState newShift newQueryIdx newEval
  where
    arityCheckOK = arityLog2 == safeLog2 (length evals)
    proofCheckOK = checkMerkleProof merkleCap (query_index_rev newQueryIdx) (flattenExt evals) proof
    evalCheckOK  = evals !! (query_index_rev oldQueryIdx `mod` arity) == oldEval
    newShift     = pow_ oldShift arity
    arity        = exp2 arityLog2 
    coset        = prepareCoset oldShift oldQueryIdx evals
    -- (newQueryIdx,newEval) = foldCosetWith beta (oldQueryIdx,coset)
    newQueryIdx = foldQueryIdx arityLog2 oldQueryIdx
    newEval     = foldCosetWith beta coset

evalPolynomialAt :: Num coeff => PolynomialCoeffs coeff -> coeff -> coeff
evalPolynomialAt (MkPolynomialCoeffs coeffs) loc = value where
  value = sum $ zipWith (*) coeffs (powersOf loc)

--------------------------------------------------------------------------------

data FriStepParams = MkFriStepParams
  { step_input_degree :: Log2
  , step_arity        :: Log2
  }
  deriving Show

expandReductionStrategy :: Log2 -> FriReductionStrategy -> [FriStepParams]
expandReductionStrategy degree_logn strategy = 
  case strategy of
    ConstantArityBits arity_bits final_poly_bits -> expandConstantStrategy arity_bits final_poly_bits degree_logn 
    Fixed arities                                -> expandFixedStrategy arities degree_logn
    _ -> error "reduction strategy not implemented"
  where

    expandConstantStrategy :: Log2 -> Log2 -> Log2 -> [FriStepParams]
    expandConstantStrategy arity_bits final_poly_bits = go where
      go logn = if logn <= final_poly_bits 
        then []
        else (MkFriStepParams logn arity_bits) : go (logn - arity_bits)

    expandFixedStrategy :: [Log2] -> Log2 -> [FriStepParams]
    expandFixedStrategy = go where
      go []     _    = []
      go (a:as) logn = (MkFriStepParams logn a) : go as (logn - a)

--------------------------------------------------------------------------------

checkFRIProof :: VerifierCircuitData -> Proof -> ProofChallenges -> Bool
checkFRIProof vkey@(MkVerifierCircuitData{..}) proof@(MkProof{..}) challenges = ok where

  MkProofChallenges{..}      = challenges
  MkCommonCircuitData{..}    = common
  MkFriChallenges{..}        = fri_challenges
  fri_proof@(MkFriProof{..}) = opening_proof

  common       = verifier_common
  fri_config   = config_fri_config circuit_config
  fri_openings = toFriOpenings openings   

  ok     = pow_ok && and oks
  pow_ok = checkProofOfWork fri_config fri_challenges
  oks    = safeZipWith checkQueryRound fri_query_indices fri_query_round_proofs

  merkleOracles = toMerkleOracles vkey proof
  precomp       = precomputeReducedOpenings fri_alpha fri_openings
  logn_lde      = fri_LDE_bits    circuit_fri_params 
  logn_degree   = fri_degree_bits circuit_fri_params
  steps_params  = expandReductionStrategy logn_degree (fri_reduction_strategy fri_config)

  checkQueryRound :: Int -> FriQueryRound -> Bool
  checkQueryRound idx this_round@(MkFriQueryRound{..}) = round_ok where

    row_evals     = checkInitialTreeProofs common merkleOracles fri_alpha idx fri_initial_trees_proof
    combined_eval = combineInitial common challenges precomp row_evals idx

    initialState = MkFoldingState
      { folding_shift         = mulGen
      , folding_query_idx     = MkQueryIndex logn_lde idx
      , folding_upstream_eval = combined_eval
      }

    foldingInputs = safeZipWith4
      (\step_params beta cap step -> MkFoldingInput (step_arity step_params) beta cap step)
      steps_params
      fri_betas 
      fri_commit_phase_merkle_caps 
      fri_steps
    
    -- note: foldingStep actually checks the steps, but for simplicity
    -- we just throw an exception if the checks fail. 
    -- TODO: maybe better error handling (though I'm fed up with this shit) 
    finalState = foldl' foldingStep initialState foldingInputs

    x_final         = folding_query_loc finalState
    final_poly_eval = evalPolynomialAt fri_final_poly (fromBase x_final)

    round_ok = final_poly_eval == folding_upstream_eval finalState

--------------------------------------------------------------------------------

