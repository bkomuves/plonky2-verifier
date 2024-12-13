
-- | Plonky2's non-standard version of the Poseidon hashs
--
-- See <https://github.com/0xPolygonZero/plonky2/blob/main/plonky2/src/hash/poseidon.rs>
--

{-# LANGUAGE Strict #-}
module Hash.Poseidon where

--------------------------------------------------------------------------------

import Data.Word

import Data.Array (Array)
import Data.Array.IArray

import Algebra.Goldilocks
import Hash.Constants
import Hash.Digest

--------------------------------------------------------------------------------

-- | permutation of @[0..11]@
kats :: [Word64]
kats = 
  [ 0xd64e1e3efc5b8e9e , 0x53666633020aaa47 , 0xd40285597c6a8825 , 0x613a4f81e81231d2
  , 0x414754bfebd051f0 , 0xcb1f8980294a023f , 0x6eb2a9e4d54a9d0f , 0x1902bc3af467e056
  , 0xf045d5eafdc6021f , 0xe4150f77caaa3be5 , 0xc9bfd01d39b50cce , 0x5c0a27fcb0e1459b
  ]

sanityCheck :: Bool
sanityCheck = (map toF kats == listPermutation (map toF [0..11]))

--------------------------------------------------------------------------------

listPermutation :: [F] -> [F]
listPermutation = elems . permutation . listToState

permutation :: State -> State
permutation 
  = finalRounds
  . internalRounds
  . initialRounds

--------------------------------------------------------------------------------

initialRounds :: State -> State
initialRounds
  = externalRound (all_ROUND_CONSTANTS ! 3) 
  . externalRound (all_ROUND_CONSTANTS ! 2) 
  . externalRound (all_ROUND_CONSTANTS ! 1) 
  . externalRound (all_ROUND_CONSTANTS ! 0) 

internalRounds :: State -> State
internalRounds = foldr1 (.) (map (internalRound $) (reverse internalRoundConstants)) where
  internalRoundConstants = take 22 $ drop 4 $ elems all_ROUND_CONSTANTS

finalRounds :: State -> State
finalRounds
  = externalRound (all_ROUND_CONSTANTS ! 29) 
  . externalRound (all_ROUND_CONSTANTS ! 28) 
  . externalRound (all_ROUND_CONSTANTS ! 27) 
  . externalRound (all_ROUND_CONSTANTS ! 26) 

--------------------------------------------------------------------------------

type RCs = Array Int F

externalRound :: RCs -> State -> State
externalRound rcs = linearDiffusion . sboxExternal rcs

internalRound :: RCs -> State -> State
internalRound rcs = linearDiffusion . sboxInternal rcs

--------------------------------------------------------------------------------

sbox1 :: F -> F
sbox1 x = pow x 7

sboxRC :: F -> F -> F
sboxRC rc x = sbox1 (x+rc)

sboxInternal :: RCs -> State -> State
sboxInternal rcs xs = listToState $ sboxInternal_ (elems rcs) (elems xs)

sboxExternal :: RCs -> State -> State
sboxExternal rcs xs = listToState $ sboxExternal_ (elems rcs) (elems xs)

sboxInternal_ :: [F] -> [F] -> [F]
sboxInternal_ (r0:rcs) (x0:xs) = sboxRC r0 x0 : zipWith (+) rcs xs

sboxExternal_ :: [F] -> [F] -> [F]
sboxExternal_ rcs xs = zipWith sboxRC rcs xs

--------------------------------------------------------------------------------

linearDiffusion :: State -> State
linearDiffusion state = listArray (0,11) $ [ sum [ mdsMatrixCoeff i j * (state!j) | j<-[0..11] ] | i<-[0..11] ] 

--------------------------------------------------------------------------------
