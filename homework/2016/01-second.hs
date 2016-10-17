-- HW 01: assgn. 2/4

-- define function to split list at given index
-- and return as two-part tuple
splitAt' :: (Monad m) [a] -> m (a, [a])
splitAt' xs n
	| length firstN < n || n < 0 = fail "invalid index given"
	| otherwise = return (firstN, lastN)
	where	firstN = take n xs
		lastN = drop n xs
