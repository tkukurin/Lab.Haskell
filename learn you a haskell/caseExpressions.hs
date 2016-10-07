
describeList :: [a] -> String
describeList xs = "The list is " ++ eval xs
	where eval [] = "empty"
	      eval [x] = "a singleton list"
	      eval xs = "neither singleton nor empty."
