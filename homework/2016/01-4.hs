-- HW 01: assgn. 4/4: get all divisors + calc first N primes

findFactor :: Int -> Int
findFactor x = head $ filter (\d -> x `mod` d == 0) [2..]

factorize :: Int -> [Int]
factorize x = filter (\d -> x `mod` d == 0) [1..x]

primes :: Int -> [Int]
primes numPrimes = take numPrimes (filter (\x -> findFactor x == x) [2..])

