
-- your version of intercalate

intercalate' :: [a] -> [[a]] -> [a]
intercalate' xs [ys] = ys
intercalate' xs yss = (head yss) ++ xs ++ intercalate' xs (tail yss)

