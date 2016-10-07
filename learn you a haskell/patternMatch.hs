factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Bono"
charName x = "Neither Albert nor Bono"

listMatch :: [a] -> a
listMatch [] = error "Empty list dude."
listMatch (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "Empty list"
tell (x:[]) = "Single element list"
tell (x:y:[]) = "Two element list"
tell otherLists = "Long lists"

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

firstLetter :: String -> String 
firstLetter "" = "Empty string"
firstLetter all@(x:xs) = "Capital in " ++ all ++ " is " ++ [x]

