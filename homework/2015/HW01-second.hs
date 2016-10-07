
-- Sort by value or lexicographically if values are equal

finalRankingSort [] = []
finalRankingSort (x:xs) =
	let lhs = finalRankingSort [ a | a <- xs, snd a > snd x || lexSmaller a x ]
	    rhs = finalRankingSort [ a | a <- xs, snd a < snd x || lexLarger a x ]
	in lhs ++ [x] ++ rhs
	where lexSmaller a x = (snd a == snd x && fst a <= fst x)
	      lexLarger a x = (snd a == snd x && fst a > fst x) 	

finalRanking :: (Integral a) => (String, a) -> (String, a) -> (String, a) -> [String]
finalRanking x y z = [ fst a | a <- finalRankingSort [ x, y, z ] ]

