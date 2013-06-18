main :: IO ()
main =
  print solution

solution :: Integer
solution =
  last $ primeFactors 600851475143

primes :: [Integer]
primes = [ x | x <- 2:[3,5..], isPrime x primes ]
isPrime :: Integer -> [Integer] -> Bool
isPrime n p =
  let candidates = takeWhile ( \x -> x * x <= n ) p
      factors = [ x | x <- candidates, n `mod` x == 0 ]
  in n == 2 || null factors

primeFactors :: Integer -> [Integer]
primeFactors num =
  factor num primes
  where
    factor n (p:ps)
      | p*p > n        = [n]
      | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
      | otherwise      = factor n ps
