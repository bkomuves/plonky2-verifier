
-- | Plonky2's Poseidon gate
--

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.Poseidon where

--------------------------------------------------------------------------------

import Data.Array hiding (range)
import Data.Char
import Data.Foldable

import Control.Monad
import Control.Monad.State.Strict

import Algebra.Goldilocks
import Algebra.Expr

import Gate.Vars
import Gate.Computation
import Hash.Constants

--------------------------------------------------------------------------------

sbox :: Expr Var_ -> Compute (Expr Var_)
sbox x = do
  x2 <- let_ "x2_" (x *x )
  x3 <- let_ "x3_" (x *x2)
  x4 <- let_ "x4_" (x2*x2)
  return (x3*x4)

flipFoldM :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m b
flipFoldM s0 list action = foldM action s0 list

flipFoldM_ :: (Foldable t, Monad m) => b -> t a -> (b -> a -> m b) -> m ()
flipFoldM_ s0 list action = void (foldM action s0 list)

-- | Poseidon state
type PS = [Expr Var_]

--------------------------------------------------------------------------------

poseidonGateConstraints :: Compute ()
poseidonGateConstraints = do

  -- merkle swap
  let input_lhs i = input  i 
  let input_rhs i = input (i+4)
  commit     $ (1 - swap_flag) * swap_flag
  commitList [ swap_flag * (input_rhs i - input_lhs i) - delta i | i <- range 4 ]

  -- swapped inputs
  let state0 :: PS
      state0 
        =  [ input_lhs  i    + delta  i     | i <- [0.. 3] ]
        ++ [ input_rhs (i-4) - delta (i-4)  | i <- [4.. 7] ] 
        ++ [ input i                        | i <- [8..11] ]

  -- initial full rounds
  phase1 <- flipFoldM state0 [0..3] $ \state1 r -> do
    let state2 = plus_rc r state1
    state3 <- if r == 0 
      then return state2
      else do
        let sbox_in = initial_sbox_in r 
        commitList [ (state2!!i) - sbox_in i | i <- range 12 ]
        return [ sbox_in i | i <- range 12 ]
    state4 <- mapM sbox state3
    return $ mds state4

  -- partial rounds
  let state'  = zipWith (+) phase1 (map LitE $ elems fast_PARTIAL_FIRST_ROUND_CONSTANT)
  let state'' = mdsInitPartial state'
  phase2 <- flipFoldM state'' [0..21] $ \state1 r -> do
    let sbox_in = partial_sbox_in r 
    commit $ (state1!!0) - sbox_in 
    y <- sbox sbox_in
    let z = if r < 21 then y + LitE (fast_PARTIAL_ROUND_CONSTANTS!r) else y
    let state2 = z : tail state1
    return $ mdsFastPartial r state2
  
  -- final full rounds
  phase3 <- flipFoldM phase2 [0..3] $ \state1 r -> do
    let state2  = plus_rc (r+26) state1
    let sbox_in = final_sbox_in r 
    commitList [ (state2!!i) - sbox_in i | i <- range 12 ]
    state3 <- mapM sbox [ sbox_in i | i <- range 12 ]
    return $ mds state3

  -- constraint the output to be the result
  commitList [ phase3!!i - output i | i <- range 12 ]

  where

    -- multiply by the MDS matrix
    mds :: PS -> PS
    mds state = 
      [ sum [ LitE (mdsMatrixCoeff i j) * x | (j,x) <- zip [0..] state ] 
      | i <- range 12 
      ]

    dotProd :: PS -> [F] -> Expr Var_
    dotProd es cs = sum $ zipWith (\e c -> e * LitE c) es cs 

    mdsInitPartial :: PS -> PS
    mdsInitPartial (first:rest) 
      = first 
      : [ sum [ LitE (partialMdsMatrixCoeff i j) * x | (j,x) <- zip [0..] rest ] 
        | i <- range 11 
        ]

    mdsFastPartial :: Int -> PS -> PS
    mdsFastPartial r state@(s0:rest) = res where
      m0  = mdsMatrixCoeff 0 0
      cs  = m0 : elems (fast_PARTIAL_ROUND_W_HATS!r)
      d   = dotProd state cs
      res = d : [ x + s0 * LitE t | (x,t) <- zip rest (elems $ fast_PARTIAL_ROUND_VS!r) ]

    -- add round constants   
    plus_rc :: Int -> PS -> PS
    plus_rc r state = zipWith (+) state (map LitE $ elems (all_ROUND_CONSTANTS!r))

    -- witness variables
    input  i  = wire i
    output i  = wire (i+12)
    swap_flag = wire 24
    delta i   = wire (25+i)
    initial_sbox_in r i = wire (29 + 12*(r-1) + i)         -- 0 <  r < 4  , 0 <= i < 12
    partial_sbox_in r   = wire (29 + 36 + r)               -- 0 <= r < 22
    final_sbox_in   r i = wire (29 + 36 + 22 + 12*r + i)   -- 0 <= r < 4  , 0 <= i < 12

--------------------------------------------------------------------------------

