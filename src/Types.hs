module Types where

import Data.Text (pack, unpack, Text, toUpper, replace, head, empty, null )

guessWords :: [String] 
guessWords = ["EMURGO", "HYDRA", "MUMBAI", "DUBAI", "TIGER", "COLORADO", "ENGLAND", "NARNIA", "MARYLAND"]

parseWord :: String -> Char 
parseWord s = Data.Text.head (toUpper (pack s))


updateemptyWord :: Int -> Char -> String -> String
updateemptyWord _ _ [] = []
updateemptyWord 0 c (x:xs) = c : xs
updateemptyWord k c (x:xs) = x : updateemptyWord (k-1) c xs