import Data.List

main :: IO ()
main =
  print solution

solution :: Int
solution =
  sum filtered
  where
    filtered = [x | x <- [1..999], mod x 3 == 0 || mod x 5 == 0]
