-- HW 01 : assgn. 1/4

-- Implement the "norm" function
norm :: (Floating a) => (a, a) -> a
norm (x, y) = sqrt (x ^ 2 + y ^ 2)


-- Define a "normalize" function
normalize :: (Monad m, Eq a, Floating a) => (a, a) -> m (a, a)
normalize (0, 0) = fail "Cannot normalize null vector"
normalize (x, y) = return (x / c, y / c)
		   where c = norm (x, y)


-- Define a "scalar multiplication" function
scalarMultiply :: (Num a) => a -> (a, a) -> (a, a)
scalarMultiply scalar (x, y) = (x * scalar, y * scalar)


-- Define a "dot product" function
dot :: (Num a) => (a, a) -> (a, a) -> a
dot (x1, y1) (x2, y2) = x1 * x2 + y1 * y2


-- (helper method)
nullVector (x, y) = x == 0 && y == 0


-- Define a "cos'" function using previous ones
cos' :: (Monad m, Eq a, Floating a) => (a, a) -> (a, a) -> m a
cos' firstVector secondVector = 
	if nullVector firstVector || nullVector secondVector 
		then fail "Null vector given"
	else
		return (dotProduct / ( normalizedFirst * normalizedSecond ))
		where dotProduct = dot firstVector secondVector 
		      normalizedFirst = norm firstVector
	      	      normalizedSecond = norm secondVector


-- Define an "are parallel" function
areParallel :: (Monad m, Eq a, Ord a, Floating a) => (a, a) -> (a, a) -> m Bool
areParallel firstVector secondVector = case cos' firstVector secondVector of
	Nothing -> fail "Null vector given"
	Just val -> return (1.0 - val <= 10e-12)

