module X16 where

-- lazy evaluation
-- innermost / outermost
-- number of reductions
-- shared arguments
-- lazy evaluation = outermost evaluation + sharing arguments

square n = n * n

f1 = square (1 + 2)

infinity = 1 + infinity

-- we will not jump into infinity !!!
f2 = fst (0, infinity)

-- basically infinite
ones = 1 : ones

-- infinite also
f3 = ones

-- terminates due to laziness (in this case, outermost evaluation)!
f4 = take 10 ones

replicate1 :: Int -> a -> [a]
replicate1 0 _ = []
replicate1 n x = x : replicate1 (n-1) x

replicate2 :: Int -> a -> [a]
replicate2 n x = take n (repeat x)

-- generate primes in the way of filtering out of multipliers
sieve (p : xs) = p : sieve [x | x <- xs, mod x p /= 0] 

primes = sieve [2..]

-- takes 23s
gt100k = head (dropWhile (<100000) primes)

-- is twin
twin (x, y) = y == x + 2

-- twin primes
twins = filter twin (zip primes (tail primes))

-- bruteforce version
is_prime1 x = head (dropWhile (<x) primes) == x

-- smarter therefore faster
is_prime2 n = not (any check primes_less_n)
              where
                check x = n `mod` x == 0
                primes_less_n = takeWhile (<n) primes
  