-- | Hangman: a word guessing game
-- Functional Programming course 2016.
-- Thomas Hallgren

import System.Random(randomRIO)
import Data.List(nub)

main = do word <- getRandomWord
          play word ""


getRandomWord :: IO String
getRandomWord = do s <- readFile "/usr/share/dict/words"
                   let words = lines s
                       n = length words
                   ix <- randomRIO (0,n-1)
                   return (words !! ix)



play :: String -> String -> IO ()
play word guesses =
    do putStr [ if c `elem` guesses then c else '_' | c<-word]
       putStrLn ("     ("++show guessesLeft++" guesses left)")
       if  all (`elem` guesses) word && guessesLeft>=0
         then putStrLn "You win!"
         else if guessesLeft<=0
              then do putStrLn "Game over!"
                      putStrLn ("The word was: "++word)
              else do putStrLn "Enter your guess:"
                      s <- getLine
                      play word (nub (guesses++s))
  where
    guessesLeft = length word + 6 - length guesses
