main :: IO ()
main =
  print solution

-- 40 choose 20 binominal number
solution :: Integer
solution =
  product [21..40] `div` product [2..20]
