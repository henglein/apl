module Data.Array.Accelerate.APL (
  Array, Scalar, Vector, Elt, Shape, Z, (:.),
  
  Nat,
  
  empty, zilde, singleton, iota, size, each, 
  shape, enlist, reshape,
  
) where
  
import Prelude hiding (map)

import Data.Array.Accelerate

type Nat = Int      -- nonnegative integers, for ranks, shapes and indexing; must be >= 0

empty :: Elt e => Acc (Vector e)
empty = use $ fromList (Z:.(0::Nat)) []

zilde :: Z
zilde = Z

singleton :: Elt e =>  Exp e -> Acc (Vector e)
singleton v = fill (index1 1) v

iota :: Exp Nat -> Acc (Vector Nat)
iota n = enumFromN (index1 n) (constant 1)

each :: (Elt a, Elt b, Shape sh) => (Exp a -> Exp b) -> Acc (Array sh a) -> Acc (Array sh b)
each = map

-- Do we need this?
-- rank :: Array sh e -> Exp Nat

enlist :: (Elt e, Shape sh) => Acc (Array sh e) -> Acc (Vector e)
enlist arr = reshape (index1 (size arr)) arr

