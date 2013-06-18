import Data.List
import ProjectEuler

main :: IO ()
main =
  print solution
  
solution :: Integer
solution =
  head $ filter ((> 500) . divisors) triangleNumbers
  where divisors n = product $ map ((+1) . length) (group (primeFactors n))
        triangleNumbers = scanl1 (+) [1..]
