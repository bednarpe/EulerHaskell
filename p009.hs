main :: IO ()
main =
  print solution

solution :: Integer
solution =
  (product . head) triplets

triplets :: [[Integer]]
triplets =
  let
    suma = 1000
    limit = floor . sqrt . fromIntegral $ suma
  in 
    [ [a,b,c] | m <- [2..limit],
                n <- [1..(m-1)], 
                let a = m^2 - n^2, 
                let b = 2*m*n, 
                let c = m^2 + n^2,
                a+b+c==suma ]
