
-- | Reference (simple, but very slow) implementation of the Goldilocks prime field

{-# LANGUAGE BangPatterns, NumericUnderscores #-}
module Goldilocks where

--------------------------------------------------------------------------------

import Prelude hiding ( div )
import qualified Prelude

import Data.Bits
import Data.Word
import Data.Ratio
import Data.Array

import Text.Printf

import System.Random

import GHC.Generics
import Data.Aeson ( ToJSON(..), FromJSON(..) )

--------------------------------------------------------------------------------

type F = Goldilocks

fromF :: F -> Word64
fromF (Goldilocks x) = fromInteger x

toF :: Word64 -> F
toF = mkGoldilocks . fromIntegral

rndF :: IO F
rndF = Goldilocks <$> randomRIO ( 0 , 0xffff_ffff_0000_0000 )

-- | Sage code:
--
-- > p = 2^64-2^32+1
-- > F = GF(p)
-- > g = F(7)
-- > print( g.multiplicative_order() == p-1 )
-- > a = g ^ ( (p-1) / 2^32 )
-- > [ a^(2^(32-k)) for k in range(33) ]
--
rootsOfUnity :: Array Int Goldilocks
rootsOfUnity = listArray (0,32) $ map toF
  [ 1
  , 18446744069414584320
  , 281474976710656
  , 18446744069397807105
  , 17293822564807737345
  , 70368744161280
  , 549755813888
  , 17870292113338400769
  , 13797081185216407910
  , 1803076106186727246
  , 11353340290879379826
  , 455906449640507599
  , 17492915097719143606
  , 1532612707718625687
  , 16207902636198568418
  , 17776499369601055404
  , 6115771955107415310
  , 12380578893860276750
  , 9306717745644682924
  , 18146160046829613826
  , 3511170319078647661
  , 17654865857378133588
  , 5416168637041100469
  , 16905767614792059275
  , 9713644485405565297
  , 5456943929260765144
  , 17096174751763063430
  , 1213594585890690845
  , 6414415596519834757
  , 16116352524544190054
  , 9123114210336311365
  , 4614640910117430873
  , 1753635133440165772
  ]

--------------------------------------------------------------------------------

newtype Goldilocks 
  = Goldilocks Integer 
  deriving (Eq,Generic)

asInteger :: Goldilocks -> Integer
asInteger (Goldilocks x) = x

instance Show Goldilocks where
  -- show (Goldilocks x) = printf "0x%016x" x
  show (Goldilocks x) = show x

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

