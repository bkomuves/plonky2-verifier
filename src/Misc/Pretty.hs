
-- | Precedence-aware pretty-printing

module Misc.Pretty where

--------------------------------------------------------------------------------

-- | See "Text.Show"
class Pretty a where
  prettyPrec :: Int -> a -> (String -> String)

pretty :: Pretty a => a -> String
pretty x = prettyPrec 0 x ""

--------------------------------------------------------------------------------
