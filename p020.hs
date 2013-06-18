import Data.Char

main :: IO ()
main =
  print solution

solution :: Int
solution = 
  sum $ map digitToInt $ show $ product [1..100] 