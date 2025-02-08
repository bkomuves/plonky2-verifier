
-- | Reference (simple, but very slow) implementation of the Goldilocks prime field

{-# LANGUAGE BangPatterns, NumericUnderscores #-}
module Algebra.Goldilocks where

--------------------------------------------------------------------------------

import Prelude hiding ( div )
import qualified Prelude

import Data.Bits
import Data.Word
import Data.List
import Data.Ratio
import Data.Array

import Text.Show
import Text.Printf

import System.Random

import GHC.Generics
import Data.Aeson ( ToJSON(..), FromJSON(..) )

import Misc.Pretty
import Misc.Aux

--------------------------------------------------------------------------------

type F = Goldilocks

fromF :: F -> Word64
fromF (Goldilocks x) = fromInteger x

toF :: Word64 -> F
toF = mkGoldilocks . fromIntegral

intToF :: Int -> F
intToF = mkGoldilocks . fromIntegral

integerToF :: Integer -> F
integerToF = mkGoldilocks 

rndF :: IO F
rndF = Goldilocks <$> randomRIO ( 0 , 0xffff_ffff_0000_0000 )

--------------------------------------------------------------------------------

-- | The generator of the multiplicative subgroup of F used by Plonky2
multGen :: F
multGen = 0xc65c18b67785d900

-- | The generator of the largest 2-adic subgroup of F used by Plonky2
twoAdicGen :: F
twoAdicGen = 0x64fdd1a46201e246 

-- | Sage code:
--
-- > p = 2^64-2^32+1
-- > F = GF(p)
-- > g = F(0xc65c18b67785d900)
-- > print( g.multiplicative_order() == p-1 )
-- > h = g ^ ( (p-1) / 2^32 )
-- > print( h == F(0x64fdd1a46201e246) )
-- > [ h^(2^(32-k)) for k in range(33) ]
--
rootsOfUnity :: Array Int Goldilocks
rootsOfUnity = listArray (0,32) $ reverse $ go twoAdicGen where
  go 1 = [1]
  go x = x : go (x*x)

subgroupGenerator :: Log2 -> F
subgroupGenerator (Log2 k) = rootsOfUnity!k

enumerateSubgroup :: Log2 -> [F]
enumerateSubgroup logSize = scanl' (\x _ -> x*g) 1 [1..n-1] where
  g = subgroupGenerator logSize
  n = exp2 logSize

--------------------------------------------------------------------------------

newtype Goldilocks 
  = Goldilocks Integer 
  deriving (Eq,Generic)

asInteger :: Goldilocks -> Integer
asInteger (Goldilocks x) = x

instance Show Goldilocks where
  show (Goldilocks x) = show x                    -- decimal
  -- show (Goldilocks x) = printf "0x%016x" x        -- hex

instance Pretty Goldilocks where prettyPrec _ x = shows x

--------------------------------------------------------------------------------

instance ToJSON Goldilocks where
  toJSON x = toJSON (asInteger x)

instance FromJSON Goldilocks where
  parseJSON o = mkGoldilocks <$> parseJSON o

--------------------------------------------------------------------------------

instance Num Goldilocks where
  fromInteger = mkGoldilocks
  negate = neg
  (+)    = add
  (-)    = sub
  (*)    = mul
  abs    = id
  signum _ = Goldilocks 1

square :: Goldilocks -> Goldilocks
square x = x*x

instance Fractional Goldilocks where
  fromRational y = fromInteger (numerator y) `div` fromInteger (denominator y)
  recip  = inv
  (/)    = div

--------------------------------------------------------------------------------

-- | @p = 2^64 - 2^32 + 1@
goldilocksPrime :: Integer
goldilocksPrime = 0x_ffff_ffff_0000_0001

modp :: Integer -> Integer
modp a = mod a goldilocksPrime

mkGoldilocks :: Integer -> Goldilocks
mkGoldilocks = Goldilocks . modp

mulGen :: F
mulGen = mkGoldilocks 0xc65c18b67785d900

--------------------------------------------------------------------------------

neg :: Goldilocks -> Goldilocks
neg (Goldilocks k) = mkGoldilocks (negate k) 

add :: Goldilocks -> Goldilocks -> Goldilocks
add (Goldilocks a) (Goldilocks b) = mkGoldilocks (a+b) 

sub :: Goldilocks -> Goldilocks -> Goldilocks
sub (Goldilocks a) (Goldilocks b) = mkGoldilocks (a-b) 

sqr :: Goldilocks -> Goldilocks
sqr x = mul x x

mul :: Goldilocks -> Goldilocks -> Goldilocks
mul (Goldilocks a) (Goldilocks b) = mkGoldilocks (a*b) 

inv :: Goldilocks -> Goldilocks
inv x = pow x (goldilocksPrime - 2)

div :: Goldilocks -> Goldilocks -> Goldilocks
div a b = mul a (inv b)

--------------------------------------------------------------------------------

pow_ :: Goldilocks -> Int -> Goldilocks
pow_ x e = pow x (fromIntegral e)

pow :: Goldilocks -> Integer -> Goldilocks
pow x e 
  | e == 0    = 1
  | e <  0    = pow (inv x) (negate e)
  | otherwise = go 1 x e
  where
    go !acc _  0     = acc
    go !acc !s !expo = case expo .&. 1 of
      0 -> go acc     (sqr s) (shiftR expo 1)
      _ -> go (acc*s) (sqr s) (shiftR expo 1)

--------------------------------------------------------------------------------

-- | @sum alpha^i * x_i@
reduceWithPowers :: Num a => a -> [a] -> a
reduceWithPowers alpha xs = go xs where
  go []      = 0
  go (!x:xs) = x + alpha * go xs

--------------------------------------------------------------------------------
