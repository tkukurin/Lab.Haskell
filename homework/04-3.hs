
-- cycle application of functions to elements of list

cycleMap :: [a -> b] -> [a] -> [b]
cycleMap [] _  = []
cycleMap _ []  = []
cycleMap fs xs = [ (fst item) (snd item) | item <- zippedWithFunction ]
		where zippedWithFunction = zip (concat $ repeat fs) xs

