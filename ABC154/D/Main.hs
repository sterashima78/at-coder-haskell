import Data.List
import Text.Printf
main :: IO ()
main = do
  [_, pickupDice] <- getInts
  dices <- getInts
  let diceSums = scanl (+) 0 dices
  let maxSum = maximum $ diff pickupDice diceSums
  print . fromRational $ toRational ( maxSum + pickupDice ) / 2

diff diffNum = zipWith subtract <$> id <*> drop diffNum

getInts = do
  x <- getLine
  let y = (map $ read) $ words x :: [Int]
  return y
