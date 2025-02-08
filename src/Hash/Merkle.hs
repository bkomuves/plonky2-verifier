
{-# LANGUAGE BangPatterns, StrictData, RecordWildCards #-}
module Hash.Merkle where

--------------------------------------------------------------------------------

import Data.Bits

import Algebra.Goldilocks

import Hash.Digest
import Hash.Poseidon
import Hash.Sponge

import Types
import Misc.Aux

--------------------------------------------------------------------------------

-- | Compression function for Merkle trees
compress :: Digest -> Digest -> Digest
compress x y = extractDigest $ permutation $ listToState s0 where
  s0 = digestToList x ++ digestToList y ++ [0,0,0,0]

--------------------------------------------------------------------------------

reconstructMerkleRoot :: [F] -> (Int,MerkleProof) -> (Int,Digest)
reconstructMerkleRoot leaf = reconstructMerkleRoot' (sponge leaf)

reconstructMerkleRoot' :: Digest -> (Int,MerkleProof) -> (Int,Digest)
reconstructMerkleRoot' leaf_digest (leaf_idx, MkMerkleProof{..}) = go leaf_idx leaf_digest siblings where
  go !idx !leaf []          = (idx, leaf)
  go !idx !leaf (this:rest) = go idx' leaf' rest where
    idx'  = shiftR idx 1
    leaf' = if isEven idx 
      then compress leaf this
      else compress this leaf

checkMerkleProof :: MerkleCap -> Int -> [F] -> MerkleProof -> Bool
checkMerkleProof cap idx leaf proof = (cap_roots!!rootidx == root) where
  MkMerkleCap cap_roots = cap
  (rootidx, root) = reconstructMerkleRoot leaf (idx,proof)

--------------------------------------------------------------------------------

