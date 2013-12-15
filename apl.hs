-- Haskell model of APL array calculus: Vector model with higher-dimensional row-major viewing
-- Reference implementation (deviates from APL model in several respects, e.g. using 0-indexing)
-- based on standard Haskell lists

import Data.List (sortBy)

type Nat = Int      -- nonnegative integers, for ranks, shapes and indexing; must be >= 0
type Shape = [Nat]  -- shape of array, must be [0] or [d1,...dn] where di > 1 (according to Hains, Mullin '91; different in APL)

-- Vectors (rank-1 arrays)

type Vector a = [a] 

--vector :: [a] -> Vector a
--vector = id

empty :: Vector a             -- empty vector
empty = []

zilde :: Shape                -- empty numeric vector/shape
zilde = []

singleton :: a -> Vector a    -- singleton vector
singleton x = [x]

iota :: Nat -> Vector Nat     -- segment [1..n] as vector
iota n = [1..n]

size :: Vector a -> Nat       -- length of vector
size = length

--append :: Vector a -> Vector a -> Vector a  -- binary vector concatenation
--append = (++)

-- segment a vector into equal-length subvectors
-- precondition: input vector has length = multiple of segment length
segment :: Nat -> Vector a -> Vector (Vector a)
segment n [] = []
segment n is = isL : segment n isR
  where (isL, isR) = splitAt n is
        
-- map        
each :: (a -> b) -> Vector a -> Vector b  -- functorial map on vectors
each = map

each2 :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
each2 f v1 v2 = map (uncurry f) (zip v1 v2)

-- reduce vector
-- Precondition: input vector must not be zilde (empty vector)
reduce :: (a -> a -> a) -> Vector a -> a
reduce f [] = error "Nonempty vector"
reduce f (x : xs) = foldl f x xs

-- Cartesian product
prod :: Vector a -> Vector b -> Vector (a, b)
prod v1 v2 = [(x, y) | x <- v1, y <- v2]

prodWith :: (a -> b -> c) -> Vector a -> Vector b -> Vector c
prodWith f v1 v2 = [f x y | x <- v1, y <- v2]

-- 2-dimensional transpose
-- precondition: all element vectors have same size
transpose2 :: Vector (Vector a) -> Vector (Vector a)
transpose2 [] = []
transpose2 vs@(v : _) = foldr (zipWith (:)) (replicate (size v) empty) vs

-- General arrays

data Array a = Array Shape (Vector a) deriving Show
-- Invariant: For all Array s xs, product s = length xs
-- Note: product [] = 1 (neutral element of multiplication)

-- reshape vector into array of certain shape
-- precondition: shape product = size of vector
array :: Shape -> Vector a -> Array a
array = Array

-- Build a rank-1 array from vector 
array1 :: Vector a -> Array a
array1 vec = Array (singleton (size vec)) vec

-- shape: lengths of each dimension
shape :: Array a -> Shape 
shape (Array s xs) = s

-- rank: dimension of array
rank :: Array a -> Nat   
rank = length . shape

-- underlying vector in array
enlist :: Array a -> Vector a
enlist (Array s xs) = xs

-- change shape of underlying vector
-- precondition product of new shape must equal product of old shape
reshape :: Shape         -- s: the new shape
        -> Array a       -- Arbitrary array    
        -> Array a       -- Shape of result = s
reshape shapeNew = array shapeNew . enlist

-- split input sequence into arrays of given shape
-- precondition: vector size must be multiple of shape product 
chop :: Shape -> Vector a -> Vector (Array a)
chop s = each (array s) . segment (product s)

-- list outer dimension as vector of arrays
-- precondition: rank inputarray > 0
ravel :: Array a -> Vector (Array a)
ravel (Array [] _) = error "Input is scalar"
ravel (Array (n : ns) elements) = chop ns elements

-- reduce outer dimension
reduceOuter :: (Array a -> Array a -> Array a) -- reduce function on rank n-1 arrays
          -> Array a   -- Input array, rank n, treated as vector (rank 1) of arrays (of rank n-1)
          -> Array a   -- Reduced array, rank n-1; rank-0 array input gives identity
-- Precondition: rank arr > 0
reduceOuter f = reduce f . ravel

-- reduce inner dimension    
-- precondition: rank of input array > 0
reduceInner :: (a -> a -> a) -> Array a -> Array a
reduceInner f (Array shape elements) =
   let (outer, [n]) = splitAt (size shape - 1) shape
   in array outer (each (reduce f) (segment n elements))

-- Apply function to each element of array
-- Note: In APL each is enclose of map of disclose; dyadic each?
--each :: (a -> b) -> Array a -> Array b
--each f (Array shape vec) = Array shape (map f vec)


-- Scalars (rank-0 arrays)

-- turn value into a scalar (= rank-0) array
enclose :: a -> Array a 
enclose = array zilde . singleton

-- project value out of scalar array
-- precondition: input array must be of rank 0
disclose :: Array a -> a  
disclose (Array [] [x]) = x
disclose (Array _ _) = error "disclose: Input not a scalar"

-- splice array scalars into outer array
-- precondition: all inner arrays have equal shape  (APL pads), at least one inner array
discloseA :: Array (Array a) -> Array a   
discloseA (Array outer items) 
  = array (outer ++ inner) [ i | Array _ is <- items, i <- is ] 
    where (Array inner _ : _) = items

encloseA :: Nat             -- number of outer dimensions in result array
         -> Array a         -- input array
         -> Array (Array a) -- output array with arrays of remaining dimensions
-- precondition: n <= rank inputarray
encloseA n (Array shape elements)
  = array outer_shape (chop inner_shape elements)
    where (outer_shape, inner_shape) = splitAt n shape


-- Array transformations
          
-- n-ary Cartesian product on numbers (auxiliary function, can be written with outer product)
prods :: [[Nat]] -> [[Nat]]
prods [] = [[]]
prods (xs : yss) = [ x : ys | x <- xs, ys <- prods yss ]

-- transpose dimensions; e.g. transpose for matrices is ordinary transpose
transpose :: Array a -> Array a
transpose (Array shape elements) =
  let numberedItems = zip (map reverse (prods (map iota shape))) elements
      compFn (pos1, _) (pos2, _) = compare pos1 pos2
  in array (reverse shape) (map snd (sortBy compFn numberedItems))

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
   in array (map (\ col -> shape !! (col-1)) perm)  (map snd (foldr sort numberedItems perm))


-- Products

-- outer product
outerProd :: Array a -> Array b -> Array (a, b)
outerProd (Array s1 v1) (Array s2 v2) =
  array (s1 ++ s2) [(x, y) | x <- v1, y <- v2]
  
-- outer product with combination function, = each (uncurry f) . outerProd
outerProdWith :: (a -> b -> c) -> Array a -> Array b -> Array c
outerProdWith f (Array s1 v1) (Array s2 v2) =
  array (s1 ++ s2) [f x y | x <- v1, y <- v2]
  
-- inner product
-- precondition: length of last dimension of left array = shape of first dimension of right array
innerProd :: (c -> c -> c) -> (a -> b -> c) -> Array a -> Array b -> Array c
innerProd plus times (Array s1 v1) a2@(Array (n2 : s2') v2) =
   let (s1', [n1]) = splitAt (size s1 - 1) s1
       v1' = segment n1 v1
       v2' = transpose2 (segment (product s2') v2)
   in array (s1' ++ s2') (each (reduce plus) (prodWith (zipWith times) v1' v2'))
    
{-

take, drop
expand ("\")
+, -, 
scalar extension, e.g. [1 2 3] + 4 = [5 6 7], [[1,2,3], [4,5,6]] + [10,110] =
[[1,2,3] + 10, [4,5,6] + 110] ; arguments of + must have same length or one has to be a 

dyadic iota;
scan;
catenate;
pack (~compress);
-}

