import Data.List

main = getLine >>= print . pickThird . toNums

-- | 文字列を数値列に変換
--
-- >>> toNums "12 645 15"
-- [12,645,15]
toNums :: String -> [Int]
toNums = map read . words

-- | 三番目に大きい数を返す
-- 
-- >>> pickThird [1,2,3,4,5,6]
-- 4
pickThird :: [Int] -> Int
pickThird x = head $ drop 2 $ reverse $ sort x