import Data.Char


-- assgn 1
capitalise :: String -> [String] -> String
capitalise sent excluded_from_capitalisation = 
  unwords $ map capitalise_if_allowed (words sent)
  where capitalise_if_allowed (x:xs) = if (x:xs) `elem` excluded_from_capitalisation 
                                    then x:xs
                                    else (toUpper x):xs


-- assgn 2
sameLetterTwiceInARow :: String -> Bool
sameLetterTwiceInARow word = 
  check (head word) (tail word)
  where check _ []     = False
        check a (x:xs) = (a == x) || (check x xs)

hasVowel :: String -> Bool
hasVowel word = length(filter (\x -> x `elem` "aeiou") word) >= 4

removeNumbers :: String -> String
removeNumbers word = filter isNotNumber word
  where isNumber = (`elem` "1234567890")
        isNotNumber x = not $ isNumber x

weirdFilterApply word = not (hasVowel word)
                          && not (sameLetterTwiceInARow word)

weirdFilter :: [String] -> [String]
weirdFilter xs = filter weirdFilterApply $ map removeNumbers xs


-- assgn 3
gcd' :: Int -> Int -> Int
gcd' a b
  | b == 0 = a
  | b > a  = gcd a (b-a)
  | otherwise = gcd b (a-b)

