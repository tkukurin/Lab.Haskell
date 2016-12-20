import Data.List

allNums :: String -> Bool
allNums xs = (length xs) == (length $ filter (\x -> x `elem` "0123456789") xs)

getFirstNumber :: [String] -> Integer
getFirstNumber = read . head . filter allNums

sortTracks :: [String] -> [String]
sortTracks = map unwords . sortOn getFirstNumber . map words

numberOfPlays :: [String] -> Integer
numberOfPlays = sum . map getFirstNumber . map words

