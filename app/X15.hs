module X15 where

import Data.Char ( digitToInt, isDigit )

type Board = [Int]

initial :: Board
initial = [5,4,3,2,1]

indexes :: [Int]
indexes = [1..length initial]

isFinised :: Board -> Bool
isFinised b = all (==0) b

isValid :: Board -> Int -> Int -> Bool
isValid b row count = 
  row > 0 && 
  row <= length b && 
  count > 0 && 
  count <= b !! (row - 1)

-- todo: via take/drop + map
move :: Board -> Int -> Int -> Board
move b row count = [adjust r n | (r, n) <- indexes `zip` b]
                    where
                      adjust r n = if r == row then n - count else n

newline :: IO ()
newline = putChar '\n'

stars :: Int -> [Char]
stars n = concat (replicate n "* ")

putRow :: Int -> Int -> IO ()
putRow n nStars = do
  putStr (show n)
  putStr ": "
  putStr (stars nStars)
  newline

putBoard :: [Int] -> IO ()
putBoard b = do
  _ <- traverse (uncurry putRow) (zip indexes b)
  return ()

getDigit :: String -> IO Int
getDigit msg = do
  putStr msg
  x <- getChar
  newline
  if isDigit x
    then 
      return (digitToInt x)
    else do
      newline
      putStrLn "not a digit, again!"  
      getDigit msg

nim :: IO()
nim = play initial 1

next :: Int -> Int
next 1 = 2
next 2 = 1

play :: Board -> Int -> IO ()
play board player = do
  newline
  putBoard board
  if isFinised board then
    do
      newline
      putStr "Player "
      putStr (show (next player))
      putStrLn " won"
  else 
    do
      newline
      putStr "Player "
      putStrLn (show player)
      r <- getDigit "Enter a row number: "
      n <- getDigit "Enter number stars to remove: "
      if isValid board r n
        then
          play (move board r n) (next player)
        else
          do
            newline
            putStrLn "invalid move"
            play board player  
