main :: IO ()
main =
  print solution

solution :: Integer
solution =
  (sum [1..100]) ^ 2 - sum(map (^2) [1..100])