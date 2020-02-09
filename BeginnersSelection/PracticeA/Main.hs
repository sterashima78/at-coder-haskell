main :: IO ()
main = do
    a <- getLine
    bc <- getLine
    s <- getLine
    let strs = words (a ++ " " ++ bc)
    let nums = map read strs :: [Int]
    putStrLn $ (show $ sum nums) ++ " " ++ s