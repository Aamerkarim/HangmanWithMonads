module Prog where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.RWS (RWST, runRWST, evalRWST, execRWST, modify, tell, ask, put)
import Control.Monad.Trans.RWS.Lazy (get)
import Data.Text (pack, unpack, Text, toUpper, replace, head, empty, null )
import Data.List ( elemIndices )
import Types ( parseWord, updateemptyWord )


prog :: RWST (Int, String) [String] (String, String, Int) IO ()
prog = do
    (maxAttempts, originalWord) <- ask -- fetches max number of guesses and the chosen word to be guessed
    userInput' <- lift getLine -- gets input from user
    if userInput' == unpack empty || length userInput' > 1  -- If user enters nothing and press enter, program does nothing and asks to enter again
       then do 
         lift $ putStrLn "--Please enter 1 character --"
         prog
    else do
      (emptyWord, chosenWord, attempts) <- get -- gets state variables
      let userInput = parseWord userInput' -- Calls parseWord function to convert from string into upper case char
      tell [[userInput]] -- writer transforer log all user entered info
      let uil = elemIndices userInput chosenWord -- checks if entered Char is an element of the word and stores relevant indexes in uil
      let huil = Prelude.head uil -- extracts index as Int from the uil
      if userInput `elem` chosenWord -- If entered Char is element of the word then do the following
        then do
          lift $ putStrLn "correct guess"
          modify (\(x, y, z) -> (updateemptyWord huil userInput emptyWord, updateemptyWord huil '_' chosenWord, z)) -- updates the state. Replaces relevant _ with the entered Char inside the Word
          (emptyWord, chosenWord, attempts) <- get -- gets updated state
          lift $ print (" Current status is  " ++ emptyWord ++ ", number of wrong guesses " ++ show attempts ++ " out of " ++ show maxAttempts) --prints updated state and shows entered Char in the Word
          if emptyWord == originalWord --  if all chars are guessed correctly then terminate otherwise call prog again
            then do
              lift $ putStrLn "-----You Won-----"
          else do
            lift $ putStrLn "entert a character"
            prog 
      else do --if entered guess is wrong then do following
          lift $ putStrLn "wrong guess"
          modify (\(x, y, z)-> (x, y, z+1)) -- modify state by increasing the guesses/attempts by 1
          (emptyWord, chosenWord, attempts) <- get -- update the state
          lift $ print (" Currect status is  " ++ emptyWord ++ ", number of wrong guesses " ++ show attempts ++ " out of " ++ show maxAttempts) -- print the updated state
          if (emptyWord, chosenWord, attempts) == (emptyWord, chosenWord, maxAttempts) -- if max attempts are exhausted then terminate otherwise call prog again
            then do
                lift $ putStrLn "-----You Lost-----"
          else do
            lift $ putStrLn "entert a character"
            prog