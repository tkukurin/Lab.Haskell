
exampleComprehension = [ let square x = x * x in (square 5, square 10) ]

calcBmis :: (RealFloat a) => [(a,a)] -> [a]
calcBmis xs = [ bmi | (w,h) <- xs, let bmi = w / h^2 ]

