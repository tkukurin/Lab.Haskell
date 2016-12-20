import Data.List

allNums :: String -> Bool
allNums xs = (length xs) == (length $ filter (\x -> x `elem` "0123456789") xs)

getTrackNo :: [String] -> Int
getTrackNo xs = (read $ head $ filter allNums xs) :: Int

sortTracks :: [String] -> [String]
sortTracks = map unwords . sortOn getTrackNo . map words

