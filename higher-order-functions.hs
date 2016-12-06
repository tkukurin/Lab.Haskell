import Data.Char

--import Prelude hiding {map,filter}
--import Data.List hiding {map,filter}

-- Int -> (Int -> (Int -> Int))
-- right-associative

-- (((number 2) 3) 4)
-- left-associative
-- => most generic arguments leftmost

halve = (/2)
valve = (/3)

-- section
partiallyAppliedInfix = (2 +)
another = (- 3)

addToList = (++ [1,2,3])
makeIntoOperatorInSectionUsingTicks = (`elem` [1,2,3])

-- "eta reduction"
isUpperLetter = (`elem` ['A'..'Z'])


-- ex.
takeThree = take 3
dropThree = drop 3
hundredTimes n = (take 100) $ repeat n
-- also: replicate 100

-- zip so every list element has its index
-- associated as fst/snd
index = (zip [0..])
index' = (`zip` [0..])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

applyOnLast f xs ys = f (last xs) (last ys)
addThree = (+3)
lastTwoPlus100 xs ys = (addThree 97) + (applyOnLast (+) xs ys)

applyManyTimes 0 f x = x
applyManyTimes 1 f x = f x
applyManyTimes n f x = f $ applyManyTimes (n-1) f x


listifyList = map (:[])
cutoff n = takeWhile (/=n)





