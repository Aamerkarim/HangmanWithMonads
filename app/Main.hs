module Main where

import Prog ( prog )
import Types ( guessWords )
import Control.Monad.Trans.RWS (execRWST)
import System.Random ( getStdRandom, Random(randomR) )

main :: IO ()
main = do
        hword <- getStdRandom (randomR (0, length guessWords-1)) -- gets a random Int based on the size of array of strings containing words to be guessed
        let chosenWord = guessWords !! hword -- randomly chooses word from array of words
        let maxAttempts = 7  -- Max number of wrong guesses 
        let emptyWord = concat (replicate (length chosenWord) "_") -- Replaces each char in string with _ 
        putStrLn $ " Word to be entered  " ++ show emptyWord
        putStrLn " Enter a character "
        ((finalEntry,guessedWords,userTries),log) <- execRWST prog (maxAttempts, chosenWord) (emptyWord, chosenWord,0)
        putStrLn $ "Actual Word was " ++ chosenWord ++ "  and you entered  " ++ show finalEntry ++ ". Total wrong guesses were " ++ show userTries ++ " and you tried the following characters " ++ show log
