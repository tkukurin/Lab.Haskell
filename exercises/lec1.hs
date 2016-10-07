concat3 :: String -> String -> String -> String
concat3 a b c = 
	a ++ (if length b < 2 then "" else b) ++ c

showSalary salary bonus =
	if salary < 0 
		then "Salary must be positive"
		else show salary ++ " " ++ show bonus
