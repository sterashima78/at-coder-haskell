import Control.Monad
import Control.Applicative

main = getLine >>= getSellValues . read >>= mapM putStrLn . toReport

getSellValues :: Int -> IO [Int]
getSellValues day = map read <$> replicateM day getLine


-- | レポート情報に変換する
-- 
-- >>> toReport [1,3,3,3,2]
-- ["up 2","stay","stay","down 1"]
toReport ::  [Int] -> [String]
toReport x = map judgeReport $ zipWith (-) (drop 1 x) x

-- | レポート情報に変換する
-- 
-- >>> judgeReport 0
-- "stay"
-- >>> judgeReport (-1)
-- "down 1"
-- >>> judgeReport 3
-- "up 3"
judgeReport :: Int -> String
judgeReport diff
  | 0 == diff = "stay"
  | diff < 0 = "down " ++ show (abs diff)
  | otherwise = "up " ++ show diff