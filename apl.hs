-- Haskell model of APL array calculus: Vector model with higher-dimensional row-major viewing
-- Reference implementation (deviates from APL model in several respects, e.g. using 0-indexing)
-- based on standard Haskell lists

class Elmt a where
  protElmt :: a

type Nat = Int      -- nonnegative integers, for ranks, shapes and indexing; must be >= 0
type Shape = [Nat]  -- shape of array, must be [], [1] or [d1,...dn] where di > 1

toShape :: [Nat] -> Shape
toShape [] = []
toShape [1] = [1]
toShape xs = filter (== 1) xs

type Seq a = [a]    -- content sequence type of arrays

-- General arrays

data Array a = Array Shape (Seq a) -- array is sequence interpreted by shape
-- For Array s xs: product s = length xs; note: product [] = 1 (neutral element of multipl.)

shape :: Array a -> Shape 
shape (Array s xs) = s

rank :: Array a -> Nat   
rank = length . shape

-- auxiliary functions
contents :: Array a -> Seq a
contents (Array s xs) = xs     

array :: Shape -> Seq a -> Array a 
array = Array

-- Vectors (rank-1 arrays)

type Vector a = Array a   -- must have rank 1

vector :: Seq a -> Vector a
vector xs = array [length xs] xs

zilde :: Vector a         -- Empty vector
zilde = vector []

ravel :: Array a -> Vector a  -- throw away shape
ravel = vector . contents

iota :: Nat -> Vector Nat   
iota n = array [n] [1 .. n]

repeat :: Nat -> 

-- Scalars (rank-0 arrays)

type Scalar a = Array a -- must have rank 0

enclose :: a -> Scalar a -- Wraps input into a scalar
enclose x = array [] [x]

disclose :: Scalar a -> a  -- Unwraps scalar input
disclose (Array [] [x]) = x
disclose (Array _ _) = error "disclose: Input not a scalar"

-- 

-- APL: mix
discloseA :: Array (Array a) -> Array a  -- assumes all arrays have equal shape; APL pads 
discloseA (Arr outer items) 
  = Arr (outer ++ inner) [ i | Arr _ is <- items, i <- is ] 
  where 
    (Arr inner _ : _) = items
    
-- APL: split
chop :: Shape -> [a] -> [Array a]
chop s [] = []
chop s is = Arr s i : chop s is'
          where
            (i,is') = splitAt (product s) is
{-            
encloseA :: Nat -> Array a -> Array a
encloseA n (Arr shape items)
  = Arr outer_shape (chop inner_shape items)
  where
    (outer_shape, inner_shape) = splitAt n shape
-}            

-- APL: dyadic transpose with repeated dimensions yields diagonal
transpose :: Vector Nat    -- Permutation of (iota (rank arg))
          -> Array a       -- arg
          -> Array a
transpose (Arr [n] perm) (Arr shape items) =
    -- assert (n == length shape ) $
    -- Arr (permute perm shape) 
    --    (scramble ... items)
    error "transpose: Unimplemented"

reshape :: Vector Nat    -- s: the new shape
        -> Array a       -- Arbitrary array    
        -> Array a       -- Shape of result = s
reshape (Arr [n] s) (Arr s' elts)
  | null elts = error "Reshape on empty array" -- not necessary if prototypical element is avail
  | otherwise = array s (take (product s) (cycle elts))

-- APL: each is enclose of map of disclose;
-- e.g. each shape (vector [vector [1, 2, 3], vector [4, 5, 6, 7]])
-- should yield vector [vector [3], vector [4]]
-- Dyadic each
each :: (a -> b) -> Array a -> Array b
each f (Arr s xs) = Arr s (map f xs)

reduce :: (Array a -> Array a -> Array a) 
          -> Array a   -- All rank one smaller than input
          -> Array a   -- Input
          -> Array a   -- Rank one smaller than input
                       --   except that rank 0 input gives identity
reduce k z (Arr [] [item])
  = Arr [] [item]   -- Identity on rank 0
reduce k z (Arr (s:ss) items)
  = foldr k z (chop ss items)

{- to do:
take, drop
expand ("\")
+, -, 
scalar extension, e.g. [1 2 3] + 4 = [5 6 7], [[1,2,3], [4,5,6]] + [10,110] =
[[1,2,3] + 10, [4,5,6] + 110] ; arguments of + must have same length or one has to be a 
singleton; 
dyadic iota;
-}

