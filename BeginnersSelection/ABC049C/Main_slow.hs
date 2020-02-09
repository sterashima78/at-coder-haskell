import Data.Char
import Data.List
main :: IO ()
main = do
  input <- getLine
  putStrLn $ isValied input $ genStr []

-- | 評価する
--
-- >>> isValied "erasedream" $ genStr []
-- "YES"
-- >>> isValied "dreameraser" $ genStr []
-- "YES"
-- >>> isValied "dreamerer" $ genStr []
-- "NO"
-- 
isValied :: String -> [String] -> String
isValied ans cands
  | ans `elem` cands = "YES"
  | length (filteredCands) == 0 = "NO"
  | otherwise = isValied ans $ genStr filteredCands
  where
    filteredCands = filterCand ans cands


-- | 文字列の生成をする
--
-- >>> genStr []
-- ["dream","dreamer","erase","eraser"]
-- >>> genStr ["a", "b"]
-- ["adream","adreamer","aerase","aeraser","bdream","bdreamer","berase","beraser"]
-- 
genStr :: [String] -> [String]
genStr [] = ["dream", "dreamer", "erase", "eraser"]
genStr x = [ y ++ z | y <- x, z <- ["dream", "dreamer", "erase", "eraser"]]

-- | 部分文字列が、期待値にマッチする値のみを返す
--
-- >>> filterCand "dreameraser" ["dream", "dreamer", "erase", "eraser"]
-- ["dream","dreamer"]
-- >>> filterCand "dreamerdreamer" ["dreamerdreame", "dreamerdreamer", "erase", "eraser"]
-- ["dreamerdreame","dreamerdreamer"]
-- 
filterCand :: String -> [String] -> [String]
filterCand ans cands = filter isPartialMatchToAns cands
  where
    isPartialMatchToAns = isPartialMatch ans

-- | 文字列が、期待値先頭にマッチするかを判定する
--
-- >>> isPartialMatch "dreameraser" "dream"
-- True
-- >>> isPartialMatch "dreameraser" "dreamer"
-- True
-- >>> isPartialMatch "dreameraser" "erase"
-- False
-- 
isPartialMatch :: String -> String -> Bool
isPartialMatch ans cand = candLength <= ansLength && (take candLength ans) == cand  
  where
    candLength = length cand
    ansLength = length ans

