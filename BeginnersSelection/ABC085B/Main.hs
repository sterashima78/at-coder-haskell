import Data.Char
import Data.List
main :: IO ()
main = do
    [cakeNum] <- getInts
    cards <- getCakes cakeNum
    putStrLn $ show $ length $ nub cards

getCakes :: Int -> IO [Int]
getCakes 1 = do
  x <- getInts
  return x
getCakes num = do
  x <- getInts
  y <- getCakes (num - 1)
  let z = x ++ y
  return z

getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y
