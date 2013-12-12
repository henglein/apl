-- Haskell model of APL array calculus: Vector model with higher-dimensional row-major viewing
-- Reference implementation (deviates from APL model in several respects, e.g. using 0-indexing)
-- based on standard Haskell lists

import Data.List (sortBy)

class Elmt a where
  protElmt :: a

type Nat = Int      -- nonnegative integers, for ranks, shapes and indexing; must be >= 0
type Shape = [Nat]  -- shape of array, must be [0] or [d1,...dn] where di > 1 (according to Hains, Mullin '91; different in APL)

-- Vectors (rank-1 arrays)

type Vector a = [a] 

vector :: [a] -> Vector a
vector = id

zilde :: Vector a         -- empty vector
zilde = []

iota :: Nat -> Vector Nat     -- segment [1..n] as vector
iota n = [1 .. n]

size :: Vector a -> Nat
size = length

append :: Vector a -> Vector a -> Vector a
append = (++)

-- General arrays

data Array a = Array Shape (Vector a) deriving Show
-- Invariant: For all Array s xs, product s = length xs; Note: product [] = 1 (neutral element of multiplication)

-- Build a rank-1 array from vector 
array :: Vector a -> Array a
array vec = Array [size vec] vec

-- shape: lengths of each dimension
shape :: Array a -> Shape 
shape (Array s xs) = s

-- underlying vector in array
ravel :: Array a -> Vector a
ravel (Array s xs) = xs

-- rank: dimension of array
rank :: Array a -> Nat   
rank = length . shape

-- Scalars (rank-0 arrays)

-- turn value into a scalar (= rank-0) array
enclose :: a -> Array a 
enclose x = Array [] [x]

-- project value out of scalar array
-- precondition: input array must be of rank 0
disclose :: Array a -> a  
disclose (Array [] [x]) = x
disclose (Array _ _) = error "disclose: Input not a scalar"

-- splice array scalars into outer array
-- precondition: all inner arrays have equal shape  (APL pads), at least one inner array
discloseA :: Array (Array a) -> Array a   
discloseA (Array outer items) 
  = Array (outer ++ inner) [ i | Array _ is <- items, i <- is ] 
  where 
    (Array inner _ : _) = items

-- split input sequence into arrays of given shape
-- precondition: vector size must be multiple of shape product 
chop :: Shape -> Vector a -> Vector (Array a)
chop s [] = []
chop s is = Array s isL : chop s isR
          where
            (isL, isR) = splitAt (product s) is

encloseA :: Nat             -- number of outer dimensions in result array
         -> Array a         -- input array
         -> Array (Array a) -- output array with arrays of remaining dimensions
-- precondition: n <= rank inputarray
encloseA n (Array shape items)
  = Array outer_shape (chop inner_shape items)
  where
    (outer_shape, inner_shape) = splitAt n shape
            
-- n-ary Cartesian product on numbers
prods :: [[Nat]] -> [[Nat]]
prods [] = [[]]
prods (xs : yss) = [ x : ys | x <- xs, ys <- prods yss ]

-- Transpose input array based on dimension permutation
transpose :: Vector Nat    -- Permutation of (iota (rank inputarray))
          -> Array a       -- input array
          -> Array a       -- output array
-- precondition: perm must be permutation of [1..shape arr]
-- Note: transpose with repeated dimensions yields diagonal in APL
transpose perm (Array shape items) =
   let 
       numberedItems = zip (prods (map iota shape)) items -- positional numbering of items
       comp :: Nat -> ([Nat], b) -> ([Nat], b) -> Ordering
       comp n (num1, i1) (num2, i2) = compare (num1 !! (n-1)) (num2 !! (n-1)) -- n-th column comp.
       sort :: Nat -> [([Nat], b)] -> [([Nat], b)]
       sort = sortBy . comp
   in Array (map (\ col -> shape !! (col-1)) perm)  (map snd (foldr sort numberedItems perm))

-- change view of underlying vector
-- precondition product of new shape must be same as product of old shape
reshape :: Vector Nat    -- s: the new shape
        -> Array a       -- Arbitrary array    
        -> Array a       -- Shape of result = s
reshape shapeNew (Array shapeOld elts) = Array shapeNew elts

-- Apply function to each element of array
-- Note: In APL each is enclose of map of disclose; dyadic each?
each :: (a -> b) -> Array a -> Array b
each f (Array shape vec) = Array shape (map f vec)

-- 
reduce :: (Array a -> Array a -> Array a) -- reduce function on rank n-1 arrays
          -> Array a   -- Neutral element
          -> Array a   -- Input array, rank n, treated as vector (rank 1) of arrays (of rank n-1)
          -> Array a   -- Reduced array, rank n-1; rank-0 array input gives identity
-- Acts like identity on scalars (rank-0 arrays)
-- Precondition: rank arr = n+1 ==> rank z = n, k preserves rank 
reduce k z (arr @ (Array [] _)) = arr   -- Identity on rank 0
reduce k z (Array (s:ss) items)
  = foldr k z (chop ss items)



{-
take, drop
expand ("\")
+, -, 
scalar extension, e.g. [1 2 3] + 4 = [5 6 7], [[1,2,3], [4,5,6]] + [10,110] =
[[1,2,3] + 10, [4,5,6] + 110] ; arguments of + must have same length or one has to be a 
singleton; 
dyadic iota;
inner product;
outer product;
scan;
catenate;
pack (~compress);
-}

