
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
-- * Log2

-- | The base 2 logarithm of an integer
newtype Log2 
  = Log2 Int
  deriving newtype (Eq,Ord,Show,Num)

deriving instance Generic Log2

instance ToJSON   Log2 where toJSON (Log2 x) = toJSON x
instance FromJSON Log2 where parseJSON y = Log2 <$> parseJSON y

fromLog2 :: Log2 -> Int
fromLog2 (Log2 k) = k

exp2 :: Log2 -> Int
exp2 (Log2 k) = shiftL 1 k

exp2' :: Log2 -> Integer
exp2' (Log2 k) = shiftL 1 k

safeLog2 :: Int -> Log2
safeLog2 n = 
  if exp2 k == n 
    then k 
    else error "safeLog2: input is not a power of two"
  where
    k = floorLog2 n

floorLog2 :: Int -> Log2
floorLog2 = floorLog2' . fromIntegral

floorLog2' :: Integer -> Log2
floorLog2' = go where
  go 0  = -1
  go 1  = 0
  go !x = 1 + go (shiftR x 1)

--------------------------------------------------------------------------------
-- * Integers

divCeil :: Int -> Int -> Int
divCeil n k = div (n+k-1) k

divFloor :: Int -> Int -> Int
divFloor = div

----------------------------------------

isEven :: Int -> Bool
isEven n = (n .&. 1) == 0

isOdd :: Int -> Bool
isOdd n = (n .&. 1) /= 0

--------------------------------------------------------------------------------
-- * Lists

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

safeZipWith3 ::  (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
safeZipWith3 f = go where
  go (x:xs) (y:ys) (z:zs) = f x y z : go xs ys zs
  go []     []     []     = []
  go _      _      _      = error "safeZipWith3: different input lengths"

safeZipWith4 ::  (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
safeZipWith4 f = go where
  go (x:xs) (y:ys) (z:zs) (w:ws) = f x y z w : go xs ys zs ws
  go []     []     []     []     = []
  go _      _      _      _      = error "safeZipWith4: different input lengths"

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

remove1 :: [a] -> [[a]]
remove1 = map snd . select1

--------------------------------------------------------------------------------
-- * Arrays 

listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, length xs - 1) xs

arrayLength :: Array Int a -> Int
arrayLength arr = let (a,b) = bounds arr in b-a+1

--------------------------------------------------------------------------------
-- * ranges

range :: Int -> [Int]
range k = [0..k-1]

range' :: Int -> Int -> [Int]
range' a b = [a..b-1]

----------------------------------------

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
