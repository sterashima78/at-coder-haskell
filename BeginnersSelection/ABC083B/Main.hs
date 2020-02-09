import Data.Char
main :: IO ()
main = do
    [maxNum, rangeMin, rangeMax] <- getInts
    putStrLn $ show $ sum $ filter (isInnerRange rangeMin rangeMax) $ [1..maxNum]

stringToInts :: String -> [Int]
stringToInts str = map (read . return) str :: [Int]

isInnerRange :: Int -> Int -> Int -> Bool
isInnerRange rangeMin rangeMax entry = 
  rangeMin <= sumOfVal && sumOfVal <= rangeMax
  where 
    sumOfVal =  sum $ stringToInts $ show entry

getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y
