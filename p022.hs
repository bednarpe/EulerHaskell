import Data.Char
import Data.List

main :: IO ()
main =
  print =<< solution

solution :: IO Int
solution = do
  str <- readFile "p022.in"
  let list = parse str
  return . solve $ list

solve :: [String] -> Int
solve names =
  sum $ zipWith (curry multiple) [1..] (map nameValue (sort names)) 
  where
    multiple (a,b) = a*b

nameValue :: String -> Int
nameValue =
  sum . map charValue
  where
    charValue a = ord a - ord 'A' + 1 

parse :: String -> [String]
parse file =
  striping $ wordsWhen (==',') file
  where
    striping = map $ filter (/='"')

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
