-- HW 01: assgn. 2/4

-- define function to split list at given index
-- and return as two-part tuple
splitAt' :: [a] -> Int -> ([a], [a])
splitAt' xs n
	| length firstN < n = error "invalid index given"
	| otherwise = (firstN, lastN)
	where	firstN = take n xs
		lastN = drop n xs
