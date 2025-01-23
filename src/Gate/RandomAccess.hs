
-- | The @RandomAccess@ gate

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.RandomAccess where

--------------------------------------------------------------------------------

import Data.Foldable
import Control.Monad

import Algebra.Goldilocks
import Algebra.Expr

import Gate.Vars
import Gate.Computation

import Misc.Aux

--------------------------------------------------------------------------------

num_routed_columns = 80 :: Int

--------------------------------------------------------------------------------

data RandomAccessGateConfig = MkRACfg 
  { ra_num_bits            :: Int        -- ^ number of bits in the index (so the vector has width @2^n@)
  , ra_num_copies          :: Int        -- ^ how many copies of this operation is included in a row
  , ra_num_extra_constants :: Int        -- ^ number of extra cells used as in ConstantGate
  }
  deriving Show

randomAccessGateConfig :: Int -> RandomAccessGateConfig
randomAccessGateConfig num_bits = ra_cfg where
  veclen = 2 ^ num_bits
  width  = 2 + veclen
  copies = Prelude.div num_routed_columns width 
  extra  = min 2 (num_routed_columns - copies*width)
  ra_cfg = MkRACfg 
    { ra_num_bits            = num_bits
    , ra_num_copies          = copies
    , ra_num_extra_constants = extra
    }

--------------------------------------------------------------------------------

randomAccessGateConstraints :: RandomAccessGateConfig -> Compute ()
randomAccessGateConstraints (MkRACfg{..}) = do

  forM_ [0..ra_num_copies-1] $ \k -> do
 
    -- index bits are actual bits
    commitList [ bits k j * (bits k j - 1) | j<-[0..ra_num_bits-1] ]

    -- bit decomposition is correct
    let reconstr = foldr (\b acc -> 2*acc + b) 0 [ bits k j | j<-[0..ra_num_bits-1] ] 
    commit $ reconstr - index k

    let lkp_val = lookup_eq 
          [ bits   k j | j<-[0..ra_num_bits-1] ]
          [ inputs k i | i<-[0..veclen-1]       ]
    commit $ lkp_val - output k

  forM_ [0..ra_num_extra_constants-1] $ \j -> commit (cnst j - extra j)

  where
  
    veclen   = 2 ^ ra_num_bits
    width    = 2 + veclen

    bits_start_at = width * ra_num_copies + ra_num_extra_constants

    -- witness variables
    index  k   = wire $ k*width + 0
    output k   = wire $ k*width + 1
    inputs k j = wire $ k*width + 2 + j
    extra  j   = wire $ ra_num_copies * width + j
    bits   k j = wire $ bits_start_at + k*ra_num_bits + j 

    into_pairs []         = []
    into_pairs (x:y:rest) = (x,y) : into_pairs rest
    into_pairs [x]        = error "into_pairs: odd input"
    
    lookup_eq []       [z] = z
    lookup_eq (b:bits) values = lookup_eq bits $ map (\(x,y) -> x + b*(y-x)) (into_pairs values)
    lookup_eq bits     values = error $ "RandomAccessGate/lookup_eq: shouldn't happen: " ++ show (length bits, length values)

--------------------------------------------------------------------------------

testRAGate = runComputation testEvaluationVarsExt (randomAccessGateConstraints $ randomAccessGateConfig 4)

