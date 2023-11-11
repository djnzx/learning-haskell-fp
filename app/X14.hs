module X14 where

f1 :: IO Char
f1 = error "todo"

f2 :: IO ()
f2 = error "todo"

f3 :: IO Char
f3 = getChar

f4 :: Char -> IO ()
f4 = putChar

f5 :: IO String
f5 = getLine

f6 :: String -> IO ()
f6 = putStr

-- lift pure value to IO
f7 :: (Monad m) => a -> m a
f7 = return

f8 :: IO Int
f8 = return 3

-- for comprehension, aligned !, `return` yields value in the end
act :: IO String
act = do
  x <- getChar
  _ <- getChar
  y <- getChar
  return [x, y]

getLine1 :: IO String
getLine1 = do
  x <- getChar
  if x == '\n'
    then return []
    else do
      xs <- getLine1
      return (x : xs)

putStr1 :: [Char] -> IO ()
putStr1 [] = return ()
putStr1 (x : xs) = do
  putChar x
  putStr1 xs

strlen :: IO ()
strlen = do
  putStr "Enter a string:"
  xs <- getLine
  putStr "The String has "
  putStr (show (length xs))
  putStrLn " characters"
