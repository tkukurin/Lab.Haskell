
import Data.Char

type RomanNumeral = String
type RomanLiteral = Char


-- check if number is valid roman numberal

isValidLiteral :: Char -> Bool
isValidLiteral c = c `elem` "IVXLCDM"


hasValidElementCount :: RomanNumeral -> Int -> Int -> Int -> Bool
hasValidElementCount [] numVs numLs numDs = numVs <= 1 && numLs <= 1 && numDs <= 1
hasValidElementCount (x:xs) numVs numLs numDs
	= hasValidElementCount xs newNumVs newNumLs newNumDs
	where   boolToInt val expected = if val == expected then 1 else 0
		newNumDs = numDs + (boolToInt x 'D')
		newNumLs = numLs + (boolToInt x 'L')
		newNumVs = numVs + (boolToInt x 'V')



-- convert number to roman numeral 
singles :: Int -> RomanLiteral -> RomanLiteral -> RomanLiteral -> RomanNumeral
singles n lower mid higher
	| n == 0    = ""
	| n < 4     = take n $ repeatedLo
	| n == 4    = lower : mid : []
	| n < 9     = mid : (take gtOrZero $ repeatedLo)
	| n == 9    = lower : higher : []
	| otherwise = higher : []
	where 	ltOrZero = if n-5 < 0 then 0 else 5-n
		gtOrZero = if n-5 > 0 then n-5 else 0
		repeatedLo = repeat lower


toRoman :: Int -> RomanNumeral
toRoman n = 
	if numberNotInRange then error "Number cannot be represented"
	else concat $ reverse $ map (\tpl -> (snd tpl) (digitToInt $ fst tpl)) (zip rev parsers)
	where 	numberNotInRange = n >= 4000 || n <= 0
		rev = reverse $ show n
		one n = singles n 'I' 'V' 'X'	
		ten n = singles n 'X' 'L' 'C'
		hundred n = singles n 'C' 'D' 'M'
		thousand n = singles n  'M' 'E' 'E' -- E should never appear (< 4000)
		parsers = [ one, ten, hundred, thousand ]
		

-- convert from roman numeral
romanLiteralToNumber literal
	| literal == 'I' = 1
	| literal == 'V' = 5
	| literal == 'X' = 10
	| literal == 'L' = 50
	| literal == 'C' = 100
	| literal == 'D' = 500
	| literal == 'M' = 1000
	| otherwise = error "Invalid literal"


fromRoman :: RomanNumeral -> Int
fromRoman xs = count (map romanLiteralToNumber xs) 0 1001 -- ensure prev > x 
	where	count [] sum prev = sum
		count (x:xs) sum prev = count xs (if prev < x then sum + x - 2*prev
	  					  else (sum + x)) x



