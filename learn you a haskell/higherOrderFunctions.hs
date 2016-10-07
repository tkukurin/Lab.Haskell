
positiveNumberOrZero x = (max 0) x

multiplyThree x y z = x * y * z
multiplyTwo x y = multiplyThree 1 x y
singleValue x = multiplyTwo 1 x

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- e.g. applyTwice (*10) 2
applyTwice f x = f ( f x )

