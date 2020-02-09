import Data.Char
import Data.List
main :: IO ()
main = do
    [cardNum] <- getInts
    cards <- getInts
    let sortedCards = zip [0..length(cards)] (reverse.sort $ cards)
    let alice = scoreOfAlice sortedCards
    let bob = scoreOfBob sortedCards
    putStrLn $ show $ alice - bob 

scoreOfAlice :: [(Int, Int)] -> Int
scoreOfAlice x = sum $ map snd $ filter (\s -> ((fst s) `mod` 2) == 0) x

scoreOfBob :: [(Int, Int)] -> Int
scoreOfBob x = sum $ map snd $ filter (\s -> ((fst s) `mod` 2) == 1) x


getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int] -- 半角スペースで分割してInt にする
  return y
