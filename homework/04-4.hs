
-- explicitly recursive function reduce
-- list of elements to single value using seed

reduce :: (a -> b -> a) -> a -> [b] -> a
reduce _ val  []     = val
reduce f seed (x:xs) = reduce f (f seed x) xs


-- variant of reduce that assumes
-- input list contains at least one element

reduce1 :: (a -> a -> a) -> [a] -> a
reduce1 f []     = error "reduce1 got an empty list."
reduce1 f (x:xs) = reduce f x xs


-- scan, similar to reduce but returns all 
-- intermediate values

scan :: (a -> b -> a) -> a -> [b] -> [a]
scan _ val  []     = [ val ]
scan f seed (x:xs) = [ seed ] ++ scan f (f seed x) xs


-- right-to-left versions of the functions

rscan :: (a -> b -> b) -> b -> [a] -> [b]
rscan f val  []     = [ val ]
rscan f seed (x:xs) = [ f x rhead ] ++ result
                where result = rscan f seed xs
                      rhead  = head result


rreduce :: (a -> b -> b) -> b -> [a] -> b
rreduce f seed xs = head (rscan f seed xs)


rreduce1 :: (a -> a -> a) -> [a] -> a
rreduce1 _ []     = error "rreduce1 got an empty list."
rreduce1 f xs     = rreduce f (last xs) (init xs)

