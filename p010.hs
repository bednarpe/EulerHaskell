main :: IO ()
main =
  print solution

solution :: Integer
solution =
  sum $ takeWhile (< 2000000) primes

primes :: [Integer]
primes = [ x | x <- 2:[3,5..], isPrime x primes ]
isPrime :: Integer -> [Integer] -> Bool
isPrime n p =
  let candidates = takeWhile ( \x -> x * x <= n ) p
      factors = [ x | x <- candidates, n `mod` x == 0 ]
  in n == 2 || null factors
