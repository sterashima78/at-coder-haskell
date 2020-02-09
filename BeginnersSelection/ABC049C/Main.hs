import Data.Char
import Data.List
main :: IO ()
main = do
  input <- getLine
  putStrLn $ isValied [input]

-- | 評価する
--
-- >>> isValied ["erasedream"]
-- "YES"
-- >>> isValied ["dreameraser"]
-- "YES"
-- >>> isValied ["huga", "dreamerer"]
-- "NO"
-- >>> isValied ["", "hoge"]
-- "YES"
-- 
isValied :: [String] -> String
isValied ans
  | "" `elem` ans = "YES"
  | length (hasPartialCands) == 0 = "NO"
  | otherwise = isValied $ genNextCands ans ["dream","dreamer","erase","eraser"]
  where
    filteredCands = filter (\s -> length (filterCand s ["dream","dreamer","erase","eraser"]) > 0) ans
    hasPartialCands = [ x | x <- ans, y <- ["dream","dreamer","erase","eraser"], isPartialMatch x y ]

-- | 削除する
-- 
-- >>> genNextCands ["dreamer","dreamere", "dreamerer"] ["dream","dreamer"]
-- ["er","","ere","e","erer","er"]
genNextCands :: [String] -> [String] -> [String]
genNextCands ans cands = [ dropString x y | x <- ans, y <- filterCand x cands]

-- | 文字列を先頭から削除する
-- 
-- >>> dropString "erasedream" "erase"
-- "dream"
dropString :: String -> String -> String 
dropString x y = drop (length y) x


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

