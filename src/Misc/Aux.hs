
-- | Misc helper functions

{-# LANGUAGE StrictData, DeriveGeneric, DeriveAnyClass, GeneralizedNewtypeDeriving, DerivingStrategies #-}
module Misc.Aux where

--------------------------------------------------------------------------------

import Data.Array 
import Data.Bits
import Data.List

import Data.Aeson hiding ( Array , pairs )
import GHC.Generics

--------------------------------------------------------------------------------

newtype Log2 
  = Log2 Int
  deriving newtype (Eq,Ord,Show,Num)

fromLog2 :: Log2 -> Int
fromLog2 (Log2 k) = k

exp2 :: Log2 -> Int
exp2 (Log2 k) = shiftL 1 k

--------------------------------------------------------------------------------

range :: Int -> [Int]
range k = [0..k-1]

range' :: Int -> Int -> [Int]
range' a b = [a..b-1]

--------------------------------------------------------------------------------

-- | Consecutive pairs of a list
pairs :: [a] -> [(a,a)]
pairs []  = []
pairs [_] = []
pairs (x:rest@(y:_)) = (x,y) : pairs rest

safeZip :: [a] -> [b] -> [(a,b)]
safeZip = safeZipWith (,)

safeZipWith ::  (a -> b -> c) -> [a] -> [b] -> [c]
safeZipWith f = go where
  go (x:xs) (y:ys) = f x y : go xs ys
  go []     []     = []
  go _      _      = error "safeZipWith: different input lengths"

longZipWith :: a -> b -> (a -> b -> c) -> [a] -> [b] -> [c]
longZipWith x0 y0 f = go where
  go []     []     = []
  go []     ys     = map (x0 `f`) ys
  go xs     []     = map (`f` y0) xs
  go (x:xs) (y:ys) = f x y : go xs ys

-- | Partition into equal sized chunks
partition :: Int -> [a] -> [[a]]
partition k = go where
  go [] = []
  go xs = take k xs : go (drop k xs)

-- | all possible ways to select 1 element out of a (nonempy) list
select1 :: [a] -> [(a,[a])]
select1 [] = error "select1: empty list"
select1 zs = go zs where 
  go [x]     = [(x,[])]
  go (x:xs)  = (x,xs) : map (\(y,ys) -> (y,x:ys)) (go xs)

--------------------------------------------------------------------------------

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

arrayLength :: Array Int a -> Int
arrayLength arr = let (a,b) = bounds arr in b-a+1

--------------------------------------------------------------------------------

-- | The interval @[a,b)@ (inclusive on the left, exclusive on the right)
data Range = MkRange 
  { range_start :: !Int
  , range_end   :: !Int
  }
  deriving (Eq,Show,Generic)

enumerateRange :: Range -> [Int]
enumerateRange (MkRange a b) = [a..b-1]

instance FromJSON Range where parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 6 }
instance ToJSON   Range where toJSON    = genericToJSON    defaultOptions { fieldLabelModifier = drop 6 }

--------------------------------------------------------------------------------
