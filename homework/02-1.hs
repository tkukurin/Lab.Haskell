-- HW 02: assgn 1/4 operations on a matrix


-- check if well formed matrix 
isWellFormed :: [[Int]] -> Bool
isWellFormed xs = 
	nonEmptyMatrix && (and $ map (== head xsLengths) (tail xsLengths))
	where xsLengths = map length xs
	      nonEmptyMatrix = head xsLengths > 0


-- size
size :: [[Int]] -> (Int, Int)
size xs = if isWellFormed xs then (length xs, head $ map length xs)
		else error "Matrix is malformed"


-- function which gets element
getElement :: [[Int]] -> Int -> Int -> Int
getElement xs y x = head $ getCol desiredRow x
	where desiredRow = [getRow xs y]


-- function which gets row
getRow :: [[Int]] -> Int -> [Int]
getRow xs row = 
	if isValidRow then head $ drop row xs
	else error "Index out of bounds"
	where isValidRow = row >= 0 && row < (snd $ size xs)


-- function which gets column
getCol :: [[Int]] -> Int -> [Int]
getCol xs col =
	if isValidCol then map (\x -> head $ drop col x) xs
	else error "Index out of bounds"
	where isValidCol = col >= 0 && col < (fst $ size xs)


-- function which adds matrices
addMatrices :: [[Int]] -> [[Int]] -> [[Int]]
addMatrices xs ys =
	if sizesEqual then sumTwoTuplesInList zippedXsAndYs
	else error "Matrices are not of equal size"
	where byElementZip x y = zip x y
	      zippedXsAndYs = zipWith byElementZip xs ys
	      sumTwoTuplesInList xs = map (\x -> map sumTwoTuple x) xs
	      sumTwoTuple twoTuple = fst twoTuple + snd twoTuple
	      sizesEqual = size xs == size ys


-- function which transposes matrices
transpose' :: [[Int]] -> [[Int]]
transpose' [[]]    = []
transpose' [[], _] = []
transpose' xs = if isWellFormed xs then map head xs : transpose' (map tail xs)
		else error "Matrix is malformed"


-- function which multiplies two matrices
multMatrices :: [[Int]] -> [[Int]] -> [[Int]]
multMatrices xs ys =
	if compatibleMatrices then 
		[ [ sum' (xsRow i) (ysCol j) 
			| j <- [0..cols - 1] ] 
				| i <- [0..rows - 1] ]
	else error "Incompatible matrix dimensions"
	where xsRow i = getRow xs i
	      ysCol j = getCol ys j
	      sum' xs ys = sum $ map (\tpl -> fst tpl * snd tpl) $ zip xs ys
	      xsSize = size xs
	      ysSize = size ys
	      cols = snd ysSize
	      rows = fst xsSize
	      compatibleMatrices = snd xsSize == fst ysSize 
