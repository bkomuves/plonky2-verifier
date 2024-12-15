
-- | Quadratic extension of the Goldilocks field
--
-- The field is defined as @F[X] / (X^2 - 7)@ 
--
-- (@X^2 - 7@ is the smallest such irreducible polynomial over Goldilocks)
--

module Algebra.GoldilocksExt where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Ratio
import Text.Show

import Data.Aeson ( ToJSON(..), FromJSON(..) )

import Algebra.Goldilocks
import Misc.Pretty

--------------------------------------------------------------------------------

type FExt = GoldilocksExt

data GoldilocksExt 
  = MkExt !Goldilocks !Goldilocks
  deriving Eq

fromBase :: Goldilocks -> GoldilocksExt
fromBase x = MkExt x 0

instance Show GoldilocksExt where
  show (MkExt real imag) = "(" ++ show real ++ " + X*" ++ show imag ++ ")"

instance Pretty GoldilocksExt where 
  prettyPrec d (MkExt real imag) 
    | imag == 0   = prettyPrec 0 real
    | otherwise   = showParen (d > 5) 
                  $ prettyPrec 0 real . showString " + X*" . prettyPrec 0 imag

instance ToJSON GoldilocksExt where
  toJSON (MkExt a b) = toJSON (a,b)

instance FromJSON GoldilocksExt where
  parseJSON o = (\(a,b) -> MkExt a b) <$> parseJSON o

--------------------------------------------------------------------------------

instance Num FExt where
  fromInteger k = MkExt (fromInteger k) 0
  negate (MkExt r i) = MkExt (negate r) (negate i)
  (+) (MkExt r1 i1) (MkExt r2 i2) = MkExt (r1+r2) (i1+i2)
  (-) (MkExt r1 i1) (MkExt r2 i2) = MkExt (r1-r2) (i1-i2)
  (*) (MkExt r1 i1) (MkExt r2 i2) = MkExt (r1*r2 + 7*i1*i2) (r1*i2 + r2*i1)
  signum = const 1
  abs    = id

instance Fractional FExt where
  fromRational q = fromInteger (numerator q) / fromInteger (denominator q)
  recip = invExt
  (/)   = divExt

--------------------------------------------------------------------------------

sqrExt :: FExt -> FExt
sqrExt x = x*x

invExt :: FExt -> FExt
invExt (MkExt a b) = MkExt c d where
  denom = inv (a*a - 7*b*b)
  c =        a * denom
  d = negate b * denom

divExt :: FExt -> FExt -> FExt
divExt u v = u * invExt v

--------------------------------------------------------------------------------

powExt :: GoldilocksExt -> Integer -> GoldilocksExt
powExt x e 
  | e == 0    = 1
  | e <  0    = powExt (invExt x) (negate e)
  | otherwise = go 1 x e
  where
    go !acc _  0     = acc
    go !acc !s !expo = case expo .&. 1 of
      0 -> go acc     (sqrExt s) (shiftR expo 1)
      _ -> go (acc*s) (sqrExt s) (shiftR expo 1)

--------------------------------------------------------------------------------

rndExt :: IO FExt
rndExt = do
  x <- rndF
  y <- rndF
  return (MkExt x y)

--------------------------------------------------------------------------------
