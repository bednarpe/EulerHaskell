import Data.Array
import Control.Arrow

main :: IO ()
main = do
  s <- solution
  print s

solution :: IO Int
solution = do
  str <- readFile "p011.in"
  let matrix = parse str
  return . maximum . prods $ matrix 

prods :: Array (Int, Int) Int -> [Int]
prods matrix =
  [product ls | d <- [(0,1),(1,0),(1,1),(1,-1)], i <- range mBounds,
                let ids = take len $ getIndices d i,
                all inArray ids,
                let ls = map (matrix!) ids]
  where
    mBounds = bounds matrix
    inArray = inRange mBounds
    len = 4
    getIndices d = iterate (\(i,j) -> (((+) i Control.Arrow.*** (+) j) d))

parse :: String -> Array (Int, Int) Int
parse =
  listArray ((1,1),(20,20)) . map read . words
