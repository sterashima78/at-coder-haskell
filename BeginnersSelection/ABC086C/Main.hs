import Data.Char
import Data.List
main :: IO ()
main = do
  [count] <- getInts
  points <- getTimePoints count
  putStrLn $ getAnswer　(PointWithTime 0 (Point 0 0)) (=<< points)


getAnswer :: PointWithTime -> [PointWithTime] -> String
getAnswer _ [] = "YES"
getAnswer (PointWithTime t0 p0) [point:points]
  | dist(p0 p1) == (t1 - t0) = getAnswer (PointWithTime t1 p1) points
  | otherwise = "NO"
  where
    (PointWithTime t1 p1) = point

-- | Point の距離
--
-- >>> dist (Point 1 2) (Point 3 5)
-- 5
dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = distX + distY
  where
    distX = abs (x1 - x2)
    distY = abs (y1 - y2)
data PointWithTime = PointWithTime Int Point deriving Show
data Point = Point Int Int deriving Show

getTimePoints :: Int -> IO [PointWithTime]
getTimePoints 1 = do
  [t, x, y] <- getInts
  let z = [PointWithTime t (Point x y)]
  return z
getTimePoints count = do
  [t, x, y] <- getInts
  points <- getTimePoints count - 1
  let z = [(PointWithTime t (Point x y))] ++ points
  return z

getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y