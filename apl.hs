-- Haskell model of APL array calculus: Vector model with higher-dimensional row-major viewing
-- Reference implementation (deviates from APL model in several respects, e.g. using 0-indexing)
-- based on standard Haskell lists

import Data.List (sortBy)

type Nat = Int      -- nonnegative integers, for ranks, shapes and indexing; must be >= 0
type Shape = [Nat]  -- shape of array, must be [0] or [d1,...dn] where di > 1 (according to Hains, Mullin '91; different in APL)

-- Vectors (rank-1 arrays)

type Vector a = [a] 

vector :: [a] -> Vector a
vector = id

zilde :: Vector a             -- empty vector
zilde = []

singleton :: a -> Vector a    -- singleton vector
singleton x = [x]

iota :: Nat -> Vector Nat     -- segment [1..n] as vector
iota n = [1..n]

size :: Vector a -> Nat       -- length of vector
size = length

append :: Vector a -> Vector a -> Vector a  -- binary vector concatenation
append = (++)

eachVec :: (a -> b) -> Vector a -> Vector b  -- functorial map on vectors
eachVec = map

-- General arrays

data Array a = Array Shape (Vector a) deriving Show
-- Invariant: For all Array s xs, product s = length xs
-- Note: product [] = 1 (neutral element of multiplication)

-- reshape vector into array of certain shape
-- precondition: shape product = size of vector
reshapeVec :: Shape -> Vector a -> Array a
reshapeVec = Array

-- Build a rank-1 array from vector 
array :: Vector a -> Array a
array vec = Array (singleton (size vec)) vec

-- shape: lengths of each dimension
shape :: Array a -> Shape 
shape (Array s xs) = s

-- underlying vector in array
enlist :: Array a -> Vector a
enlist (Array s xs) = xs

-- segment a vector into equal-length subvectors
-- precondition: input vector has length = multiple of segment length
segment :: Nat -> Vector a -> Vector (Vector a)
segment n [] = []
segment n is = isL : segment n isR
  where (isL, isR) = splitAt n is

-- split input sequence into arrays of given shape
-- precondition: vector size must be multiple of shape product 
chop :: Shape -> Vector a -> Vector (Array a)
chop s = eachVec (reshapeVec s) . segment (product s)

-- list outer dimension as vector of arrays
-- precondition: rank inputarray > 0
ravel :: Array a -> Vector (Array a)
ravel (Array [] _) = error "Input is scalar"
ravel (Array (n : ns) elements) = chop ns elements

-- rank: dimension of array
rank :: Array a -> Nat   
rank = length . shape

-- Scalars (rank-0 arrays)

-- turn value into a scalar (= rank-0) array
enclose :: a -> Array a 
enclose = reshapeVec zilde . singleton

-- project value out of scalar array
-- precondition: input array must be of rank 0
disclose :: Array a -> a  
disclose (Array [] [x]) = x
disclose (Array _ _) = error "disclose: Input not a scalar"

-- splice array scalars into outer array
-- precondition: all inner arrays have equal shape  (APL pads), at least one inner array
discloseA :: Array (Array a) -> Array a   
discloseA (Array outer items) 
  = reshapeVec (outer ++ inner) [ i | Array _ is <- items, i <- is ] 
    where (Array inner _ : _) = items

encloseA :: Nat             -- number of outer dimensions in result array
         -> Array a         -- input array
         -> Array (Array a) -- output array with arrays of remaining dimensions
-- precondition: n <= rank inputarray
encloseA n (Array shape items)
  = reshapeVec outer_shape (chop inner_shape items)
    where (outer_shape, inner_shape) = splitAt n shape

-- n-ary Cartesian product on numbers (auxiliary function, can be written with outer product)
prods :: [[Nat]] -> [[Nat]]
prods [] = [[]]
prods (xs : yss) = [ x : ys | x <- xs, ys <- prods yss ]

-- transpose dimensions; e.g. transpose for matrices is ordinary transpose
transpose :: Array a -> Array a
transpose (Array shape elements) =
  let numberedItems = zip (map reverse (prods (map iota shape))) elements
      compFn (pos1, _) (pos2, _) = compare pos1 pos2
  in reshapeVec (reverse shape) (map snd (sortBy compFn numberedItems))

-- Transpose input array based on dimension permutation
transposeWith :: Vector Nat    -- Permutation of (iota (rank inputarray))
          -> Array a       -- input array
          -> Array a       -- output array
-- precondition: perm must be permutation of [1..shape arr]
-- Note: transpose with repeated dimensions yields diagonal in APL
transposeWith perm (Array shape items) =
   let 
       numberedItems = zip (prods (map iota shape)) items -- positional numbering of items
       comp :: Nat -> ([Nat], b) -> ([Nat], b) -> Ordering
       comp n (num1, i1) (num2, i2) = compare (num1 !! (n-1)) (num2 !! (n-1)) -- n-th column comp.
       sort :: Nat -> [([Nat], b)] -> [([Nat], b)]
       sort = sortBy . comp
   in reshapeVec (map (\ col -> shape !! (col-1)) perm)  (map snd (foldr sort numberedItems perm))

-- change view of underlying vector
-- precondition product of new shape must equal product of old shape
reshape :: Vector Nat    -- s: the new shape
        -> Array a       -- Arbitrary array    
        -> Array a       -- Shape of result = s
reshape shapeNew = reshapeVec shapeNew . enlist

-- Apply function to each element of array
-- Note: In APL each is enclose of map of disclose; dyadic each?
each :: (a -> b) -> Array a -> Array b
each f (Array shape vec) = Array shape (map f vec)

-- reduce vector
-- Precondition: input vector must not be zilde (empty vector)
reduce :: (a -> a -> a) -> Vector a -> a
reduce f [] = error "Nonempty vector"
reduce f (x : xs) = foldl f x xs

-- reduce outer dimension
reduceOuter :: (Array a -> Array a -> Array a) -- reduce function on rank n-1 arrays
          -> Array a   -- Input array, rank n, treated as vector (rank 1) of arrays (of rank n-1)
          -> Array a   -- Reduced array, rank n-1; rank-0 array input gives identity
-- Precondition: rank arr > 0
reduceOuter f = reduce f . ravel

-- reduce inner dimension    
-- precondition: rank od input array >= 1
reduceInner :: (a -> a -> a) -> Array a -> Array a
reduceInner f (Array shape elements) =
   let (outer, [n]) = splitAt (length shape - 1) shape
   in reshapeVec outer (eachVec (reduce f) (segment n elements))

-- outer product
outerProd :: Array a -> Array b -> Array (a, b)
outerProd (Array s1 v1) (Array s2 v2) =
  Array (s1 ++ s2) [(x, y) | x <- v1, y <- v2]
  
-- outer product with combination function, ~ each (uncurry f) . outerProd
outerProdWith :: (a -> b -> c) -> Array a -> Array b -> Array c
outerProdWith f (Array s1 v1) (Array s2 v2) =
  Array (s1 ++ s2) [f x y | x <- v1, y <- v2]
  
-- inner product
-- precondition: length of last dimension of left array = shape of first dimension of right array
{-
innerProd plus times (Array s1 v1) (Array s2 v2) =
  

   shapeMerge [] _ = error "Left argument is a scalar"
   shapeMerge _ [] = error "Right argument is a scalar"
   shapeMerge [m] (n : ns) = if m == n then ns else error "Dimension length mismatch"
   shapeMerge (m : ms) ns = m : shapeMerge ms ns
    

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

