
-- | Monadic interface to do Fiat-Shamir challenges

{-# LANGUAGE StrictData, GeneralizedNewtypeDeriving #-}
module Challenge.Monad where

--------------------------------------------------------------------------------

import Data.Array

import Control.Monad
import Control.Monad.Identity
import qualified Control.Monad.State.Strict as S
import Control.Monad.IO.Class

import Algebra.Goldilocks
import Hash.Digest
import Challenge.Pure ( DuplexState, Squeeze, Absorb )
import qualified Challenge.Pure as Pure

--------------------------------------------------------------------------------
-- * Monadic interface

newtype DuplexT m a 
  = DuplexT (S.StateT DuplexState m a)
  deriving (Functor,Applicative,Monad)

type Duplex a = DuplexT Identity a

runDuplexT :: Monad m => DuplexT m a -> State -> m a
runDuplexT (DuplexT action) ini = S.evalStateT action (Pure.duplexInitialState ini)

runDuplex :: Duplex a -> State -> a
runDuplex action ini = runIdentity (runDuplexT action ini)

absorb :: (Monad m, Absorb a) => a -> DuplexT m ()
absorb x = DuplexT $ S.modify (Pure.absorb x)

squeeze :: (Monad m, Squeeze a) => DuplexT m a
squeeze = DuplexT $ S.state Pure.squeeze

squeezeN :: (Monad m, Squeeze a) => Int -> DuplexT m [a]
squeezeN n = DuplexT $ S.state (Pure.squeezeN n)

-- | For debugging only
inspectDuplexState :: Monad m => DuplexT m (DuplexState) 
inspectDuplexState = DuplexT S.get

--------------------------------------------------------------------------------

type DuplexIO a = DuplexT IO a

instance MonadIO (DuplexT IO) where 
  liftIO action = DuplexT (liftIO action)

duplexPrint :: Show a => a -> DuplexIO ()
duplexPrint x = DuplexT (liftIO $ print x)

printDuplexState :: DuplexIO ()
printDuplexState = duplexPrint =<< inspectDuplexState

--------------------------------------------------------------------------------

duplexTest :: Int -> IO ()
duplexTest m = runDuplexT action zeroState where
  action :: DuplexIO ()
  action = do
    forM_ [0..19] $ \k -> do
      absorb (map intToF [1..k])
      ys <- squeezeN k :: DuplexIO [F]
      duplexPrint ys

--------------------------------------------------------------------------------
