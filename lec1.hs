-- .lhs / .hs

import Data.Char
import Data.List

guardFn number 
	| number < 0 = "negative"
	| otherwise = "zero or positive"

x = 2

guardDist x y
	| d > 10 = "further than 10"
	| otherwise = "closer or equal to 10"
	where d = sqrt x ^ 2 + y ^ 2

initials :: String -> String -> String
initials first last = [f] ++ "." ++ [l] ++ "."
	where (f:_) = first
	      (l:_) = last

calcBmis :: (RealFloat a) => [(a, a)] -> a
calcBmis tupleList = [ bmi w h | (w, h) <- tupleList ]
	where bmi width height = width / height ^ 2
