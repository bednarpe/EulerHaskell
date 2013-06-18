data WeakDay = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type MonthDay = Int

data Month = January | February | March | April | May | June | July | August |
             September | October | November | December
             deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Year = Int

data Date = Date {md :: MonthDay, m :: Month, y :: Year, wd :: WeakDay}

instance Show Date where
  show (Date mds ms ys wds) = show ys ++ "-" ++
                              (show . (+1) . fromEnum) ms ++ "-"++
                              show mds ++ " " ++
                              show wds 

instance Eq Date where
  (Date mds1 ms1 ys1 wds1) == (Date mds2 ms2 ys2 wds2) =
    mds1 == mds2 && ms1 == ms2 && ys1 == ys2
  (Date mds1 ms1 ys1 wds1) /= (Date mds2 ms2 ys2 wds2) =
    mds1 /= mds2 || ms1 /= ms2 || ys1 /= ys2

instance Ord Date where
  d1 < d2 =
    numberedDate d1 < numberedDate d2
  d1 <= d2 =
    numberedDate d1 <= numberedDate d2
  d1 > d2 =
    numberedDate d1 > numberedDate d2
  d1 >= d2 =
    numberedDate d1 >= numberedDate d2

--unnumberedDate :: Int -> Date
--unnumberedDate input =
--  Date {md=0, m=0, y=0, }

numberedDate :: Date -> Integer
numberedDate (Date mds ms ys wds) =
  read $ show ys ++ (show . (+10) . fromEnum) ms ++ show mds 

instance Enum Date where
  toEnum   = undefined
  fromEnum = undefined
  succ (Date mds ms ys wds)
    | mds == 28 &&
      ms == February &&
      not (leapYear ys)     = nextMonth -- normal february
    | mds == 29 &&
      ms == February        = nextMonth -- leap february
    | mds == 31 &&
      ms == December        = nextYear -- long month
    | mds == 31 ||
      (mds == 30 &&
       elem ms shortMonths) = nextMonth
    | otherwise             = nextDay -- normal
    where
      shortMonths = [April, June, September, November]
      leapYear x = (mod x 4 == 0 && mod x 100 /= 0) || mod x 400 == 0
      nextYear = Date {md=1, m=next ms, y=ys+1, wd=next wds}
      nextMonth = Date {md=1, m=next ms, y=ys, wd=next wds}
      nextDay = Date {md=mds + 1, m=ms, y=ys, wd=next wds}
  enumFrom = undefined
  enumFromTo start end
    | start == end = [start]
    | otherwise    = start:enumFromTo nextDay end
    where
      nextDay = if start < end then
                  succ start
                else
                  pred start
  enumFromThen = undefined
  enumFromThenTo = undefined

next :: (Enum a, Bounded a) => a -> a
next = turn 1

--prev :: (Enum a, Bounded a) => a -> a
--prev = turn (-1)

turn :: (Enum a, Bounded a) => Int -> a -> a
turn n e = toEnum (add (fromEnum (maxBound `asTypeOf` e) + 1) (fromEnum e) n)
    where
      add mod x yx = (x + yx + mod) `rem` mod

main :: IO ()
main =
  print solution

solution :: Int
solution = do
  --countSundays 1 1 1900
  let known = Date {md=1, m=January, y=1900, wd=Monday}
  let start' = Date {md=31, m=December, y=1900, wd=undefined}
  let start = succ $ last [known .. start']
  let end' = Date {md=31, m=December, y=2000, wd=undefined}
  let end = last [start .. end']
  --print end
  --return 0
  countSundays [start..end]
  --print end
  --return 0
  where
    countSundays = countSundays' 0
    countSundays' n [] = n
    countSundays' n ( Date mds ms ys wds :ds)
      | wds == Sunday && mds == 1 = countSundays' (n + 1) ds
      | otherwise                 = countSundays' n ds

