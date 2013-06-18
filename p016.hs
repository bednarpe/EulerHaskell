import Data.Char

main :: IO ()
main =
  print solution

solution :: Int
solution =
  sum $ map digitToInt $ show $ 2^1000
