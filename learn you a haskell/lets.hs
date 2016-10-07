
ringo :: String -> String
ringo x =
	let suffixRingo = " was the drummer"
	    suffixOther = " was not the drummer"
	in if x == "Ringo" 
		then x ++ suffixRingo
		else x ++ suffixOther

letVal1 = (let a = 100; b = 20 in a * b)
letVal2 = (let (a,b,c) = (1,2,3) in a+b+c)

