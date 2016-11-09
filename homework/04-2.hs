
-- split list into sublists of length n

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = [take n xs] ++ chunk n (drop n xs)


-- split into sublists of lengths as given

chunkBy :: [Int] -> [a] -> [[a]]
chunkBy [] _      = []
chunkBy _ []      = []
chunkBy (x:xs) ys = if x > 0 then [take x ys] ++ remainder
                             else remainder
                    where remainder = chunkBy xs (drop x ys)


-- chunk into n sublists of equal length

chunkInto :: Int -> [a] -> [[a]]
chunkInto 0 xs = []
chunkInto n xs = if partLen * n /= fromIntegral (length xs)
                    then take (n-1) chunked ++ [ concat (drop (n-1) chunked) ]
                    else chunked
        where partLen = (quot (length xs) n)
              chunked = chunk partLen xs



