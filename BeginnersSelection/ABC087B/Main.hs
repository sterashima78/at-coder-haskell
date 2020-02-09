main :: IO ()
main = do
    [coin500] <- getInts
    [coin100] <- getInts
    [coin50] <- getInts
    [ans] <- getInts
    let all = [ (a, b, c) | a <- [0..coin500], b <- [0..coin100], c <- [0..coin50]]
    putStrLn $ show $ length $ filter (\s -> s == ans )$ map sumCoin all

sumCoin :: (Int, Int, Int) -> Int
sumCoin (c500, c100, c50) = 500 * c500 + 100 * c100 + 50 * c50

toInt :: (String -> Int)
toInt x = read x ::Int

getInts = do
  x <- getLine
  let y = (map $ toInt) $ words x -- 半角スペースで分割してInt にする
  return y
