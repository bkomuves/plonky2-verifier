
-- | Fiat-Shamir challenges

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module Challenge where

--------------------------------------------------------------------------------

import Data.Array

import Control.Monad
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class

import Goldilocks
import GoldilocksExt
import Poseidon
import Digest
import Types

--------------------------------------------------------------------------------

-- | Plonky2 uses the duplex sponge construction with overwrite mode
data DuplexState 
  = Absorbing { duplexOld :: State, duplexInp :: [F] } 
  | Squeezing { duplexOld :: State, duplexOut :: [F] }
  deriving (Eq,Show)

duplexInitialState :: State -> DuplexState
duplexInitialState state = Absorbing state [] 

overwrite :: [F] -> State -> State
overwrite new old = listToState $ new ++ drop (length new) (elems old)

duplex :: [F] -> State -> State
duplex inp old = permutation (overwrite inp old)

extract :: State -> [F]
extract state = reverse $ take rate (elems state) where 
  rate = 8

freshSqueezing :: State -> DuplexState
freshSqueezing new = Squeezing new (extract new)

--------------------------------------------------------------------------------

absorbFelt :: Goldilocks -> DuplexState -> DuplexState
absorbFelt x mode = 
  case mode of
    Squeezing old _   -> absorbFelt x (Absorbing old [])
    Absorbing old inp -> if length inp < rate
      then                Absorbing             old (inp ++ [x]) 
      else absorbFelt x $ Absorbing (duplex inp old) [] 
  where
    rate = 8

squeezeFelt :: DuplexState -> (Goldilocks, DuplexState)
squeezeFelt mode =
  case mode of
    Squeezing old out -> case out of
      []     -> let new = permutation old
                in  squeezeFelt $ freshSqueezing new 
      (y:ys) -> (y, Squeezing old ys)
    Absorbing old inp -> case inp of
      []     -> squeezeFelt $ freshSqueezing (permutation old) 
      (x:xs) -> squeezeFelt $ freshSqueezing (duplex  inp old) 

--------------------------------------------------------------------------------

class Absorb a where
  absorb :: a -> DuplexState -> DuplexState

instance Absorb F where
  absorb = absorbFelt

instance Absorb FExt where absorb (MkExt a b) = absorb [a,b]

instance Absorb a => Absorb [a] where
  absorb []     = id
  absorb (x:xs) = absorb xs . absorb x

instance Absorb Digest where absorb h = absorb (digestToList h)

instance Absorb MerkleCap where
  absorb (MkMerkleCap digests) = absorb digests

--------------------------------------------------------------------------------

class Squeeze a where
  squeeze :: DuplexState -> (a, DuplexState)

squeezeN :: Squeeze a => Int -> DuplexState -> ([a], DuplexState)
squeezeN 0 state0 = ([],state0)
squeezeN n state0 = let (x   , state1) = squeeze        state0 
                        (xs  , state2) = squeezeN (n-1) state1
                    in  (x:xs, state2)

instance Squeeze Goldilocks where squeeze = squeezeFelt

instance Squeeze GoldilocksExt where 
  squeeze state0 = 
    let (x, state1) = squeeze state0
        (y, state2) = squeeze state1
    in  (MkExt x y, state2)

--------------------------------------------------------------------------------
-- * Monadic interface

newtype DuplexT m a 
  = DuplexT (S.StateT DuplexState m a)
  deriving (Functor,Applicative,Monad)

type DuplexIO a = DuplexT IO a

instance MonadIO (DuplexT IO) where 
  liftIO action = DuplexT (liftIO action)

runDuplexT :: Monad m => DuplexT m a -> State -> m a
runDuplexT (DuplexT action) ini = S.evalStateT action (duplexInitialState ini)

doPrint :: Show a => a -> DuplexIO ()
doPrint x = DuplexT (liftIO $ print x)

doDebug :: DuplexIO ()
doDebug = DuplexT $ do
  state <- S.get
  liftIO (print state)
  return ()

doAbsorb :: Absorb a => a -> DuplexIO ()
doAbsorb x = DuplexT $ S.modify (absorb x)

doSqueeze :: Squeeze a => DuplexIO a
doSqueeze = DuplexT $ S.state squeeze

doSqueezeN :: Squeeze a => Int -> DuplexIO [a]
doSqueezeN n = DuplexT $ S.state (squeezeN n)

--------------------------------------------------------------------------------

duplexTest :: Int -> IO ()
duplexTest m = flip runDuplexT zeroState $ do
  forM_ [0..19] $ \k -> do
    doAbsorb (map intToF [1..k])
    ys <- doSqueezeN k :: DuplexIO [F]
    doPrint ys

--------------------------------------------------------------------------------
