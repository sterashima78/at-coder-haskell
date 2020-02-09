main :: IO ()
main = do
    [num1, num2] <- map read . words <$> getLine 
    putStrLn $ judge num1 num2
 
judge :: Int -> Int -> String
judge x y
    | x `mod` 2 == 0 = "Even"
    | y `mod` 2 == 0 = "Even"
    | otherwise = "Odd"