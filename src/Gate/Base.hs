
-- | Custom gates

{-# LANGUAGE StrictData, PackageImports, DeriveGeneric, DeriveAnyClass #-}
module Gate.Base where

--------------------------------------------------------------------------------

import Data.Word

import Data.Aeson
import GHC.Generics

import Algebra.Goldilocks

--------------------------------------------------------------------------------

newtype KeccakHash
  = MkKeccakHash [Word8]
  deriving (Eq,Show,Generic)

instance ToJSON   KeccakHash where toJSON (MkKeccakHash hash) = toJSON hash
instance FromJSON KeccakHash where parseJSON o = MkKeccakHash <$> parseJSON o

--------------------------------------------------------------------------------

data Gate
  = ArithmeticGate          { num_ops    :: Int }
  | ArithmeticExtensionGate { num_ops    :: Int }
  | BaseSumGate             { num_limbs  :: Int , base :: Int }
  | CosetInterpolationGate  { subgroup_bits :: Int, coset_degree :: Int , barycentric_weights :: [F] }
  | ConstantGate            { num_consts :: Int }
  | ExponentiationGate      { num_power_bits :: Int }
  | LookupGate              { num_slots  :: Int, lut_hash :: KeccakHash }
  | LookupTableGate         { num_slots  :: Int, lut_hash :: KeccakHash, last_lut_row :: Int }
  | MulExtensionGate        { num_ops    :: Int }
  | NoopGate
  | PublicInputGate
  | PoseidonGate            { hash_width :: Int}
  | PoseidonMdsGate         { hash_width :: Int}
  | RandomAccessGate        { num_bits :: Int, num_copies :: Int, num_extra_constants :: Int }
  | ReducingGate            { num_coeffs :: Int }
  | ReducingExtensionGate   { num_coeffs :: Int }
  | UnknownGate  String
  deriving (Eq,Show,Generic)

--------------------------------------------------------------------------------
