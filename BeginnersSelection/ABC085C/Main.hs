import Data.Char
import Data.List
main :: IO ()
main = do
    [num, ans] <- getInts
    putStrLn $ toString $ filter (isValied ans) $ [(x, y ,num-x-y) | x <- [0..num], y <- [0..num], x + y <= num]

toString :: [(Int, Int, Int)] -> String
toString [] = "-1 -1 -1"
toString [(a, b, c)] = show c ++ " " ++ show b ++ " " ++ show a
toString x = toString $ [head x]
  

isValied :: Int -> (Int, Int, Int) -> Bool
isValied sumValue (c1000, c5000, c10000) = (c1000 * 1000 + c5000 * 5000 + c10000 * 10000) == sumValue

getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y
