
-- | The @CosetInterpolation@ gate

{-# LANGUAGE StrictData, RecordWildCards #-}
module Gate.Custom.CosetInterp where

--------------------------------------------------------------------------------

import Data.Foldable
import Control.Monad

import Algebra.Goldilocks
import Algebra.GoldilocksExt
import Algebra.Expr

import Gate.Vars
import Gate.Computation

import Misc.Aux

--------------------------------------------------------------------------------

data CosetInterpolationGateConfig = MkCICfg 
  { ci_subgroup_bits       :: Log2       -- ^ logarithm of the size of the subgroup (typically 4)
  , ci_degree              :: Int        -- ^ equation degree (?)
  , ci_barycentric_weights :: [F]        -- ^ barycentric weights
  }
  deriving Show

cosetInterpolationGateConfig :: Log2 -> CosetInterpolationGateConfig
cosetInterpolationGateConfig subgroup_bits = ci_cfg where
  ci_cfg = MkCICfg 
    { ci_subgroup_bits       = subgroup_bits
    , ci_degree              = degree
    , ci_barycentric_weights = calcBarycentricWeights coset
    }
  max_degree       = 8
  n_points         = exp2 subgroup_bits
  n_intermed_guess = (n_points - 2) `Prelude.div` (max_degree - 1)              -- ???
  degree           = (n_points - 2) `Prelude.div` (n_intermed_guess + 1) + 2    -- ???
  coset            = enumerateSubgroup subgroup_bits

-- | See <https://en.wikipedia.org/wiki/Lagrange_polynomial#Barycentric_form>
calcBarycentricWeights :: [F] -> [F]
calcBarycentricWeights locations = weights where
  weights = map recip $ map f $ select1 locations
  f (x,ys) = product [ x - y | y<-ys ]

--------------------------------------------------------------------------------

cosetInterpolationGateConstraints :: CosetInterpolationGateConfig -> Compute ()
cosetInterpolationGateConstraints (MkCICfg{..}) = do

  let MkExt u v = eval_loc - scaleExt coset_shift shifted_loc
  commitList [ u , v ]
 
  let initials = initial : [ (tmp_eval i , tmp_prod i) | i <- [0..n_intermediates-1] ]
  let chunks   = zip3 chunked_domain chunked_values chunked_weights

  let worker ini (d,v,w) = partial_interpolate d v w ini
  let stuff    = zipWith worker initials chunks

  forM_ (zip [0..] (init stuff)) $ \(i,(eval,prod)) -> do
    let MkExt u1 v1 = tmp_eval i - eval
    let MkExt u2 v2 = tmp_prod i - prod
    commitList [ u1 , v1 , u2 , v2 ]
 
  let (final_eval,_) = last stuff
  let MkExt u v = eval_result - final_eval
  commitList [ u , v ]

  where
  
    max_degree      = 8 
    degree          = ci_degree
    n_points        = exp2 ci_subgroup_bits
    n_intermediates = (n_points - 2) `Prelude.div` (ci_degree - 1)  

    domain = enumerateSubgroup ci_subgroup_bits     :: [F]
    values = [ poly_value k | k <- range n_points ] :: [Ext Expr_]

    chunked_domain  = chunk domain
    chunked_values  = chunk values
    chunked_weights = chunk ci_barycentric_weights

    -- witness variables
    coset_shift  = wire 0                                                   :: Expr_
    poly_value k = wireExt $ 1 + 2*k                                        :: Ext Expr_
    eval_loc     = wireExt $ 1 + 2*n_points                                 :: Ext Expr_
    eval_result  = wireExt $ 1 + 2*n_points + 2                             :: Ext Expr_
    tmp_eval i   = wireExt $ 1 + 2*(n_points+2) + 2*i                       :: Ext Expr_
    tmp_prod i   = wireExt $ 1 + 2*(n_points+2) + 2*(n_intermediates + i)   :: Ext Expr_
    shifted_loc  = wireExt $ 1 + 2*(n_points+2) + 4* n_intermediates        :: Ext Expr_
 
    initial = (MkExt 0 0 , MkExt 1 0) :: (Ext Expr_, Ext Expr_)

    -- we use this formula <https://en.wikipedia.org/wiki/Lagrange_polynomial#Barycentric_form>
    -- but in 1) chunks and 2) iteratively
    --
    -- this is what happens:
    --
    -- run0 = 0
    -- run1 = run0*(x - x1) + val1*1
    -- run2 = run1*(x - x2) + val2*(x - x1)
    -- run3 = run2*(x - x3) + val3*(x - x1)*(x - x2)
    -- run4 = run3*(x - x4) + val4*(x - x1)*(x - x2)*(x - x3)
    -- run5 = run4*(x - x5) + val5*(x - x1)*(x - x2)*(x - x3)*(x - x4)
    --
    -- this computes barycentric formula (val_i already conntains the barycentric weights)
    --
    partial_interpolate :: [F] -> [Ext Expr_] -> [F] -> (Ext Expr_, Ext Expr_) -> (Ext Expr_, Ext Expr_)
    partial_interpolate domain values weights ini = result where
      weighted = zipWith scaleExt (map LitE weights) values
      result   = foldl f ini (zip weighted domain)
      x0       = shifted_loc

      f :: (Ext Expr_, Ext Expr_) -> (Ext Expr_, F) -> (Ext Expr_, Ext Expr_)
      f (eval,prod) (val, xi) = (next_eval,next_prod) where
        term = x0 - fromBase (LitE xi)
        next_eval = term * eval + val * prod
        next_prod = term * prod 

    -- the first chunk has degree one less so it can have length one more...
    chunk xs = take degree xs : partition (degree-1) (drop degree xs) 

--------------------------------------------------------------------------------

