main :: IO ()
main =
  print solution

solution :: Integer
solution =
  maximum [z | x <- [100..999], y <- [x..999], let z = x*y, let s = show z, s == reverse s]