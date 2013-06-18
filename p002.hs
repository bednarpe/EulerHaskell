import Data.IORef

type Generator = IO Int

main :: IO ()
main =
  do
    s <- solution
    print s

solution :: IO Int
solution =
  do
    nextFib <- fibGenerator
    solutionLoop nextFib 0

solutionLoop :: Generator -> Int -> IO Int
solutionLoop gen suma =
  do
    new <- gen
    if new >= 4000000 then
      return suma
    else
      solutionLoop gen (if even new then suma + new else suma)

fibGenerator :: IO Generator
fibGenerator = do
  m <- newIORef (1, 2)
  return (do
             mem <- readIORef m
             modifyIORef' m fibStep
             return $ fst mem)
  where fibStep t = (snd t, uncurry (+) t)
