-- assgn. 2/4


-- find shortest distance to visit all pubs
-- (test version, todo refactor)
manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan start end =
	(abs $ (fst start) - (fst end)) + (abs $ (snd start) - (snd end))


extractNth start xs n = (nth, manhattan start nth, (init lWithNth) ++ r) 
	where 	lWithNth = take n xs
		r = drop n xs
		nth = last lWithNth

minList [x]    = x
minList (x:xs) = min x (minList xs)


shortestDistance :: (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Int -> Int
shortestDistance start []   current currentSum = currentSum + (manhattan start current)
shortestDistance start pubs current currentSum =
	minList $ [ shortestDistance start (getC node) (getA node) (currentSum + (getB node)) 
			| node <- nodes ]
	where 	nodes = [ extractNth current pubs val | val <- [1..length pubs] ]
		distances = [ manhattan start pub | pub <- pubs ]
		getA (a,b,c) = a
		getB (a,b,c) = b
		getC (a,b,c) = c	
