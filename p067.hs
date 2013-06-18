import Data.Char

main :: IO ()
main =
  print =<< solution

solution :: IO Int
solution = do
  str <- readFile "p067.in"
  let triangle = parse str
  return . solve $ triangle

solve :: [[Int]] -> Int
solve =
  head . foldr1 f
  where
    f [] [s] = [s]
    f (s:sl) (x:y:zl) = s + max x y : f sl (y:zl)

parse :: String -> [[Int]]
parse =
  map (map read . words) . lines
