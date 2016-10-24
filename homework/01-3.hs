-- HW 01: assgn. 3/4: implements the Luhn algorithm

import Data.Char (digitToInt)

-- helpers
doubleEverySecondFromListEnd :: [Int] -> [Int]
doubleEverySecondFromListEnd xs =
	let zipped = zip (reverse xs) [2..] 
	in reverse [ (y `mod` 2 + 1) * x | (x, y) <- zipped ]


integerifyByChar :: String -> [Int]
integerifyByChar xs = map digitToInt xs


luhnSumSingleValue :: Int -> Int
luhnSumSingleValue x = xTens + xSingles
	where 	xSingles = x `mod` 10
		xTens = x `quot` 10


luhnSumList :: [Int] -> Int
luhnSumList xs = sum $ map luhnSumSingleValue xs


getLuhnChecksum :: String -> Int
getLuhnChecksum str = luhnSumList
			$ doubleEverySecondFromListEnd 
			$ integerifyByChar str


isValidLuhnChecksum :: String -> Bool
isValidLuhnChecksum str = getLuhnChecksum str `mod` 10 == 0



