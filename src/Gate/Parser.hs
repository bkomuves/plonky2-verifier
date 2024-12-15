
-- | Gates are encoded as strings produced by some ad-hoc modifications 
-- of the default Rust textual serialization...
--
-- ... so we have to parse /that/
--
-- (and then figure out what equations do they imply)
--

{-# LANGUAGE StrictData, PackageImports, DeriveGeneric, DeriveAnyClass #-}
module Gate.Parser where

--------------------------------------------------------------------------------

import Data.Word

import Data.Aeson ( FromJSON(..) , ToJSON(..) )
import GHC.Generics

import "parsec1" Text.ParserCombinators.Parsec

import Algebra.Goldilocks
import Gate.Base

--------------------------------------------------------------------------------

instance FromJSON Gate where 
  parseJSON o = recognizeGate <$> parseJSON o

--------------------------------------------------------------------------------
-- * Parsing Rust gate strings

integerP :: Parser Integer
integerP = read <$> many1 digit

intP :: Parser Int
intP = fromInteger <$> integerP

byteP :: Parser Word8
byteP = fromInteger <$> integerP

fieldP :: Parser F
fieldP = mkGoldilocks <$> integerP

commaP :: Parser ()
commaP = do
  char ','
  spaces
  return ()

listP :: Parser a -> Parser [a]
listP userP = do
  char '[' ; spaces
  ys <- sepBy userP commaP
  char ']' ; spaces
  return ys

fieldListP :: Parser [F]
fieldListP = listP fieldP

keccakHashP :: Parser KeccakHash
keccakHashP = MkKeccakHash <$> listP byteP

--------------------------------------------------------------------------------

withEOF :: Parser a -> Parser a
withEOF userP = do
  y <- userP
  eof
  return y

rustStructP :: String -> Parser a -> Parser a
rustStructP name userP = do
  string name ; spaces
  char '{'    ; spaces
  y <- userP  ; spaces
  char '}'    ; spaces
  return y

keyValueP :: String -> Parser a -> Parser a
keyValueP key userP = do
  string key  ; spaces
  char ':'    ; spaces
  y <- userP  ; spaces
  return y

oneP :: (String, Parser a) -> Parser a
oneP (key1,user1) = do
  x <- keyValueP key1 user1
  return x

twoP :: (String, Parser a) -> (String, Parser b) -> Parser (a,b)
twoP (key1,user1) (key2,user2) = do
  x <- keyValueP key1 user1 ; commaP
  y <- keyValueP key2 user2
  return (x,y)

threeP :: (String, Parser a) -> (String, Parser b) -> (String, Parser c) -> Parser (a,b,c)
threeP (key1,user1) (key2,user2) (key3,user3) = do
  x <- keyValueP key1 user1 ; commaP
  y <- keyValueP key2 user2 ; commaP
  z <- keyValueP key3 user3
  return (x,y,z)

--------------------------------------------------------------------------------

recognizeGate :: String -> Gate
recognizeGate str = case runParser gateP () "<gate>" str of
  Left  err  -> error (show err)
  Right gate -> gate

gateP :: Parser Gate
gateP 
  =   try arithmeticGateP
  <|> try arithmeticExtensionGateP
  <|> try baseSumGateP
  <|> try cosetInterpolationGateP
  <|> try constantGateP
  <|> try exponentiationGateP
  <|> try lookupGateP
  <|> try lookupTableGateP
  <|> try mulExtensionGateP
  <|> try noopGateP
  <|> try publicInputGateP
  <|> try poseidonGateP
  <|> try poseidonMdsGateP
  <|> try randomAccessGateP
  <|> try reducingGateP
  <|> try reducingExtensionGateP
  <|> (UnknownGate <$> many anyToken)

--------------------------------------------------------------------------------

arithmeticGateP :: Parser Gate
arithmeticGateP = withEOF $ rustStructP "ArithmeticGate" $ do
  ArithmeticGate <$> oneP ("num_ops", intP)

arithmeticExtensionGateP :: Parser Gate
arithmeticExtensionGateP = withEOF $ rustStructP "ArithmeticExtensionGate" $ do
  ArithmeticExtensionGate <$> oneP ("num_ops", intP)

-- "BaseSumGate { num_limbs: 63 } + Base: 2"
baseSumGateP :: Parser Gate
baseSumGateP = withEOF $ do
  limbs <- rustStructP "BaseSumGate" $ oneP ("num_limbs", intP)
  char '+' ; spaces
  base <- oneP ("Base", intP)
  return $ BaseSumGate limbs base

-- "CosetInterpolationGate { subgroup_bits: 4, degree: 6, barycentric_weights: [17293822565076172801, ... ]], _phantom: PhantomData<plonky2_field::goldilocks_field::GoldilocksField> }<D=2>"
cosetInterpolationGateP :: Parser Gate
cosetInterpolationGateP = withEOF $ do
  gate <- rustStructP "CosetInterpolationGate" $ do
    (x,y,z) <- threeP
      ("subgroup_bits"       , intP      )
      ("degree"              , intP      )
      ("barycentric_weights" , fieldListP)
    commaP
    string "_phantom: PhantomData<plonky2_field::goldilocks_field::GoldilocksField>"
    spaces
    return $ CosetInterpolationGate x y z
  string "<D=2>"
  return gate

constantGateP :: Parser Gate
constantGateP = rustStructP "ConstantGate" $ do
  ConstantGate <$> oneP ("num_consts" , intP)

exponentiationGateP :: Parser Gate
exponentiationGateP = rustStructP "ExponentiationGate" $ do
  ExponentiationGate <$> oneP ("num_power_bits" , intP)

lookupGateP :: Parser Gate
lookupGateP = rustStructP "LookupGate" $ do
  (x,y) <- twoP
    ("num_slots"   , intP        )
    ("lut_hash"    , keccakHashP )
  return $ LookupGate x y

lookupTableGateP :: Parser Gate
lookupTableGateP = rustStructP "LookupTableGate" $ do
  (x,y,z) <- threeP
    ("num_slots"   , intP        )
    ("lut_hash"    , keccakHashP )
    ("last_lut_row", intP        )
  return $ LookupTableGate x y z

mulExtensionGateP :: Parser Gate
mulExtensionGateP = rustStructP "MulExtensionGate" $ do
  MulExtensionGate <$> oneP ("num_ops", intP)

noopGateP :: Parser Gate
noopGateP = string "NoopGate" >> return NoopGate

publicInputGateP :: Parser Gate
publicInputGateP = string "PublicInputGate" >> return PublicInputGate

-- "RandomAccessGate { bits: 4, num_copies: 4, num_extra_constants: 2, _phantom: PhantomData<plonky2_field::goldilocks_field::GoldilocksField> }<D=2>"
randomAccessGateP :: Parser Gate
randomAccessGateP = do
  (x,y,z) <- rustStructP "RandomAccessGate" $ do
    xyz <- threeP
      ("bits"               , intP )
      ("num_copies"         , intP )
      ("num_extra_constants", intP )
    commaP
    string "_phantom: PhantomData<plonky2_field::goldilocks_field::GoldilocksField>"
    spaces
    return xyz
  string "<D=2>"
  return $ RandomAccessGate x y z

-- "PoseidonGate(PhantomData<plonky2_field::goldilocks_field::GoldilocksField>)<WIDTH=12>"
poseidonGateP :: Parser Gate
poseidonGateP = do
  string "PoseidonGate(PhantomData<plonky2_field::goldilocks_field::GoldilocksField>)<WIDTH="
  w <- intP
  string ">"
  eof
  return $ PoseidonGate w

poseidonMdsGateP :: Parser Gate
poseidonMdsGateP = do
  string "PoseidonMdsGate(PhantomData<plonky2_field::goldilocks_field::GoldilocksField>)<WIDTH="
  w <- intP
  string ">"
  eof
  return $ PoseidonMdsGate w

reducingGateP :: Parser Gate
reducingGateP = rustStructP "ReducingGate" $ do
  arg <- oneP ("num_coeffs" , intP)
  optional $ string "<D=2>"
  return (ReducingGate arg)

reducingExtensionGateP :: Parser Gate
reducingExtensionGateP = rustStructP "ReducingExtensionGate" $ do
  arg <- oneP ("num_coeffs" , intP)
  optional $ string "<D=2>"
  return (ReducingExtensionGate arg)

--------------------------------------------------------------------------------
