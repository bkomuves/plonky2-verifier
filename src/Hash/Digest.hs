
-- | Hash digests consist of 4 goldilocks field elements

{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Hash.Digest where

--------------------------------------------------------------------------------

import Data.Array
import Data.Word
import Data.Bits

import GHC.Generics
import Data.Aeson ( FromJSON(..) , ToJSON(..) , object , withObject , (.=) , (.:) )

import Algebra.Goldilocks

--------------------------------------------------------------------------------

type State = Array Int F

listToState' :: Int -> [F] -> State
listToState' n = listArray (0,n-1)

listToState :: [F] -> State
listToState = listToState' 12

zeroState' :: Int -> State
zeroState' n = listToState' n (replicate n 0)

zeroState :: State
zeroState = zeroState' 12

--------------------------------------------------------------------------------

data Digest 
  = MkDigest !F !F !F !F
  deriving (Eq,Show,Generic)

instance ToJSON Digest where
  toJSON (MkDigest a b c d) = object [ "elements" .= toJSON [a,b,c,d] ]

instance FromJSON Digest where
  parseJSON = withObject "Digest" $ \obj -> listToDigest <$> obj .: "elements"

zeroDigest :: Digest
zeroDigest = MkDigest 0 0 0 0

extractDigest :: State -> Digest
extractDigest state = case elems state of 
  (a:b:c:d:_) -> MkDigest a b c d

extractCapacity :: State -> [F]
extractCapacity state = case elems state of 
  (_:_:_:_:_:_:_:_:a:b:c:d:[]) -> [a,b,c,d]

listToDigest :: [F] -> Digest
listToDigest [a,b,c,d] = MkDigest a b c d

digestToList :: Digest -> [F]
digestToList (MkDigest a b c d) = [a,b,c,d]

--------------------------------------------------------------------------------

digestToWord64s :: Digest -> [Word64]
digestToWord64s (MkDigest a b c d) = [ fromF a, fromF b, fromF c, fromF d]

digestToBytes :: Digest -> [Word8]
digestToBytes = concatMap bytesFromWord64LE . digestToWord64s

--------------------------------------------------------------------------------

bytesFromWord64LE :: Word64 -> [Word8]
bytesFromWord64LE = go 0 where
  go 8  _  = []
  go !k !w = fromIntegral (w .&. 0xff) : go (k+1) (shiftR w 8)

bytesToWord64LE :: [Word8] -> Word64
bytesToWord64LE = fromInteger . bytesToIntegerLE

bytesToIntegerLE :: [Word8] -> Integer
bytesToIntegerLE = go where
  go []          = 0 
  go (this:rest) = fromIntegral this + 256 * go rest

--------------------------------------------------------------------------------
