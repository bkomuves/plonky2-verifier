
module Hash where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word
import Data.Bits

import Algebra.Goldilocks
import Poseidon
import Digest

--------------------------------------------------------------------------------

-- | Poseidon sponge construction (rate=8, capacity=4) without padding.
--
-- Notes:
-- 
-- * Plonky2 to use the sponge in \"overwrite mode\"
--
-- * No padding is applied (inputs are expected to be fixed length)
--
sponge :: [F] -> Digest
sponge = go zeroState where
  go !state [] = extractDigest state
  go !state xs = case splitAt 8 xs of
    (this,rest) -> go (permutation $ combine this state) rest
  combine xs arr = listToState $ xs ++ drop (length xs) (elems arr)

-- | Sponge with @10*1@ padding. The only place this is used is hashing
-- the domain separator (which is empty by default)
spongeWithPad :: [F] -> Digest
spongeWithPad what = go zeroState (what ++ [1]) where
  go !state [] = extractDigest state
  go !state xs = case splitAt 8 xs of
    (this,rest) -> go (permutation $ combine this state) rest
  combine xs arr = let k = length xs in if k < 8 
    then listToState $ xs ++ replicate (8-k-1) 0 ++ [1] ++ drop 8 (elems arr)
    else listToState $ xs ++ drop k (elems arr)

-- | Compression function for Merkle trees
compress :: Digest -> Digest -> Digest
compress x y = extractDigest $ permutation $ listToState s0 where
  s0 = digestToList x ++ digestToList y ++ [0,0,0,0]

--------------------------------------------------------------------------------

