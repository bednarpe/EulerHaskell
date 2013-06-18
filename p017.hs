import Data.Char

main :: IO ()
main =
  print solution

solution :: Int
solution =
  sum $ map toWordsLen [1..1000]

toWordsLen :: Int -> Int
toWordsLen x
  | x < 20    = s!!x
  | x < 100   = m!! div x 10 + toWordsLen (mod x 10)
  | x == 1000 = 11 -- onethousand
  | x >= 100  = 7 + toWordsLen (div x 100) + (if mod x 100 == 0 then
                                                      0
                                                    else 3 + toWordsLen (mod x 100))
  | otherwise = 0
  where
    s = [0,3,3,5,4,4,3,5,5,4,3,6,6,8,8,7,7,9,8,8]
    --small [zero,one,two,three,four,five,six,seven,eight,nine,ten,
    --       eleven,twelve,thirteen,fourteen,fifteen,
    --       sixteen,seventeen,eighteen,nineteen]
    m = [0,3,6,6,5,5,5,7,6,6]
    --medium [zero,ten,twenty,thirty,forty,fifty,sixty,seventy,eighty,ninety]
    --l = [7]
    --hundred 
