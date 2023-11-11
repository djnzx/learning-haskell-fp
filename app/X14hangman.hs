module X14hangman where

import System.IO (hSetEcho, stdin)

hangman :: IO ()
hangman = do
  putStrLn "Think a word:"
  word <- getLineSecure
  putStrLn "Guess it!"
  play word

getLineSecure :: IO String
getLineSecure = do
  x <- getCharNoEcho
  if x == '\n'
    then do
      putChar x
      return []
    else do
      putChar '-'
      xs <- getLineSecure
      return (x : xs)

getCharNoEcho :: IO Char
getCharNoEcho = do
  hSetEcho stdin False
  x <- getChar
  hSetEcho stdin True
  return x

play :: String -> IO ()
play word = do
  putStrLn "? "
  guess <- getLine
  if guess == word
    then putStrLn "You got it"
    else do
      putStrLn (match word guess)
      play word

--       origin    guess  printed output
match :: String -> String -> String
match xs ys = [if x `elem` ys then x else '-' | x <- xs]
