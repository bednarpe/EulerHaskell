-- {-# OPTIONS_GHC +RTS -N -RTS #-}
--import Control.Parallel
--import qualified Data.MemoCombinators as Memo
import Data.List
--import Data.Ord

main :: IO ()
main =
  print solution
  
solution :: Integer
solution =
  snd $ foldl1' max [(collatzLen n, n) | n <- [2..999999]]
  --where
    --collatzMax l r = if snd l > snd r then l else r 

collatzLen :: Integer -> Int
collatzLen = collatzLen' 1

collatzLen' :: Int -> Integer -> Int
collatzLen' c 1 = c
collatzLen' c n = collatzLen' (c+1) $ if n `mod` 2 == 0 then n `div` 2 else 3*n+1
