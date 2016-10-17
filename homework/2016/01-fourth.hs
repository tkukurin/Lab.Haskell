-- HW 01: assgn. 4/4: get all divisors + calc first N primes

findFactor :: Int -> Int -> Int
findFactor x tryDivisor
	| x `mod` tryDivisor == 0 = tryDivisor
	| otherwise = findFactor x (tryDivisor - 1)


factorize :: Int -> [Int]
factorize x = _factorize x x []


_factorize :: Int -> Int -> [Int] -> [Int]
_factorize x currentTryDivisor factorsSoFar
	| currentFactor == 1 = 1:factorsSoFar
	| otherwise = _factorize x (currentFactor - 1) (currentFactor:factorsSoFar)
	where	currentFactor = findFactor x currentTryDivisor
		

primes :: Int -> [Int]
primes numPrimes = take numPrimes (filter (\x -> findFactor x (x - 1) == 1) [2..])

