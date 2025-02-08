
{-# LANGUAGE BangPatterns, StrictData #-}
module Algebra.FFT where

--------------------------------------------------------------------------------

import Data.Array
import Data.List
import Data.Bits
import Data.Word

import Algebra.Goldilocks
import Algebra.GoldilocksExt

import Misc.Aux

--------------------------------------------------------------------------------

-- | Reverse the order of bits in an n-bit word
reverseBits :: Log2 -> Word64 -> Word64
reverseBits (Log2 n) w = foldl' (.|.) 0
  [ shiftL ((shiftR w k) .&. 1) (n-k-1) | k<-[0..n-1] ]

reverseBitsInt :: Log2 -> Int -> Int
reverseBitsInt log2 = fromIntegral . reverseBits log2 . fromIntegral

reverseIndexBitsNaive :: Array Int a -> Array Int a
reverseIndexBitsNaive arr1 = arr2 where
  (0,n1)        = bounds arr1
  log2@(Log2 k) = safeLog2 (n1 + 1)
  arr2 = array (0,n1) [ (reverseBitsInt log2 i , x) | (i,x) <- assocs arr1 ]

reverseIndexBits :: Array Int a -> Array Int a
reverseIndexBits = reverseIndexBitsNaive

reverseIndexBitsList :: [a] -> [a]
reverseIndexBitsList = elems . reverseIndexBitsNaive . listToArray

--------------------------------------------------------------------------------

powersOf' :: Num a => a -> a -> [a]
powersOf' !start !g = go start where go !x = x : go (g*x)

powersOf :: Num a => a -> [a]
powersOf = powersOf' 1 

--------------------------------------------------------------------------------

{-
ifft :: [FExt] -> [FExt]
ifft xs = go kk xs where
  nn   = length xs
  kk   = safeLog2 nn
  g    = subgroupGenerator kk
  ginv = recip g
  hs   = powersOf' half ginv
  half = 1 / 2 :: F

  go (Log2 0) [x]   = [ x ]
  go (Log2 1) [x,y] = [ (x+y)/2 , (x-y)/2 ]
  go (Log2 k) input = 
    case splitAt halfN input of
      (xs,ys) -> go km1 (zipWith  f1 xs ys   ) ++ 
                 go km1 (zipWith3 f2 hs xs ys)
    where
      f1    !x !y = scaleExt half (x + y)
      f2 !h !x !y = scaleExt h    (x - y)
      halfN = exp2 km1
      km1   = Log2 k - 1
-}

--------------------------------------------------------------------------------

naiveFFT :: [FExt] -> [FExt]
naiveFFT xs = ys where
  nn   = length xs
  kk   = safeLog2 nn
  g    = subgroupGenerator kk
  ys = [ sum [ scaleExt (g^(j*k)) (xs!!j) | j<-[0..nn-1] ]
       | k <- [0..nn-1]
       ]

naiveIFFT :: [FExt] -> [FExt]
naiveIFFT xs = ys where
  nn   = length xs
  kk   = safeLog2 nn
  g    = subgroupGenerator kk
  ginv = recip g
  fn   = toF (fromIntegral nn)
  ys   = map (scaleExt (recip fn)) 
           [ sum [ scaleExt (ginv^(j*k)) (xs!!j) | j<-[0..nn-1] ]
           | k <- [0..nn-1]
           ]

--------------------------------------------------------------------------------
