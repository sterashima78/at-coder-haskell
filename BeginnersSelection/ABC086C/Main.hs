import Data.Char
import Data.List
main :: IO ()
main = do
  [count] <- getInts
  points <- getTimePoints count
  let initPoint = PointWithTime 0 (Point 0 0)
  putStrLn $ getAnswer initPoint points


-- | 答えの導出
-- 
-- >>> getAnswer (PointWithTime 0 (Point 0 0)) [(PointWithTime 1 (Point 0 1)), (PointWithTime 3 (Point 2 1))]
-- "Yes"
-- >>> getAnswer (PointWithTime 0 (Point 0 0)) [(PointWithTime 1 (Point 0 1)), (PointWithTime 4 (Point 1 1))]
-- "Yes"
-- >>> getAnswer (PointWithTime 0 (Point 0 0)) [(PointWithTime 1 (Point 0 1)), (PointWithTime 5 (Point 0 1))]
-- "Yes"
-- >>> getAnswer (PointWithTime 0 (Point 0 0)) [(PointWithTime 1 (Point 0 1)), (PointWithTime 3 (Point 2 2))]
-- "No"

-- >>> getAnswer (PointWithTime 0 (Point 0 0)) [(PointWithTime 2 (Point 100 100))]
-- "No"
getAnswer :: PointWithTime -> [PointWithTime] -> String
getAnswer _ [] = "Yes"
getAnswer (PointWithTime t0 p0) (point:points)
  | isUnderMax && (maxDist - dist01) `mod` 2 == 0 = getAnswer point points
  | otherwise = "No"
  where
    (PointWithTime t1 p1) = point
    dist01 = dist p0 p1
    maxDist = t1 - t0
    isUnderMax = dist01 <= maxDist


-- | Point の距離
--
-- >>> dist (Point 1 2) (Point 3 5)
-- 5
-- >>> dist (Point 3 5) (Point 2 1)
-- 5
-- >>> dist (Point 4 4) (Point 4 4)
-- 0
dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = distX + distY
  where
    distX = abs (x1 - x2)
    distY = abs (y1 - y2)
data PointWithTime = PointWithTime Int Point deriving Show
data Point = Point Int Int deriving Show

getTimePoints :: Int -> IO [PointWithTime]
getTimePoints 1 = getTimePoint
getTimePoints count = do
  point <- getTimePoint
  points <- getTimePoints $ count - 1
  let x = point ++ points
  return x

getTimePoint :: IO [PointWithTime]
getTimePoint = do
  [t, x, y] <- getInts
  return [PointWithTime t (Point x y)]

getInts = do
  x <- getLine
  let y = map read $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y