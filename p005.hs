main :: IO ()
main =
  print solution

solution :: Int
solution =
  foldr1 lcm [1..20]