

type Probability = Double
type DiscreteRandVar = [(Int, Probability)]


x :: DiscreteRandVar
x = [(1, 0.2), (2, 0.4), (3, 0.1), (4, 0.2), (5, 0.05), (6, 0.05)]


-- define explicitly recursive function mean
-- and accumulator-style function mean'

multiplyIntAndDouble :: Int -> Double -> Double
multiplyIntAndDouble a b = (fromIntegral a) * b


mean :: DiscreteRandVar -> Double
mean []      = 0
mean (x:xs)  = multiplyIntAndDouble (fst x) (snd x) + mean xs 


mean' :: DiscreteRandVar -> Double
mean' xs = meanAcc xs 0.0
	where   meanAcc [] sum     = sum
		meanAcc (x:xs) sum = meanAcc xs (sum + multiplyIntAndDouble (fst x) (snd x))


-- define explicitly recursive variance
-- and accumulator-style variance'

variance :: DiscreteRandVar -> Double
variance xs = varianceDelegate xs (mean xs)
	where 	varianceDelegate [] _ = 0
		varianceDelegate (x:xs) m = getVal x m + varianceDelegate xs m
		getVal x m = (fromIntegral (fst x) - m)^2 * snd x


variance' :: DiscreteRandVar -> Double
variance' xs = varianceDelegate xs (mean xs) 0
	where 	varianceDelegate [] _ sum = sum
		varianceDelegate (x:xs) m sum = varianceDelegate xs m (sum + getVal x m)
		getVal x m = (fromIntegral (fst x) - m)^2 * snd x


-- define probabilityFilter and probabilityFilter'

probabilityFilter :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter p []     = []
probabilityFilter p (x:xs) =
	(if snd x >= p then [fst x] else []) ++ probabilityFilter p xs


probabilityFilter' :: Probability -> DiscreteRandVar -> [Int]
probabilityFilter' p xs = probabilityFilterDelegate p xs []
	where 	probabilityFilterDelegate _ [] accum = accum
		probabilityFilterDelegate p (x:xs) accum = 
			probabilityFilterDelegate p xs 
				(accum ++ if snd x >= p then [fst x] else [])


