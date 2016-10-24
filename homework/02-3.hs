-- HW 02: assgn 3/4 Number N divisible by 30

import Data.Char (digitToInt)
import Data.List

largestMultiple :: String -> Int
largestMultiple nr =
	if notDivisibleBy3 || notDivisibleBy10 then error "No such number"
	else read $ sortBy (flip compare) nr
	where notDivisibleBy3 = (sum $ map digitToInt nr) `mod` 3 /= 0
	      notDivisibleBy10 = not $ '0' `elem` nr

