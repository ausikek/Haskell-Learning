module HigherOrderFuncs where

    square :: Int -> Int
    square x = x * x

    -- map
    squareList :: [Int] -> [Int]
    squareList x = map square x

    -- foldr
    sumOfSquares :: [Int] -> Int
    sumOfSquares x = foldr (+) 0 (squareList x)

    greaterThanZero :: Int -> Bool
    greaterThanZero x
        | x > 0 = True
        | otherwise = False

    -- filter
    keepNonZeroes :: [Int] -> [Int]
    keepNonZeroes x = filter greaterThanZero x

    -- definir map usando foldr
    map2 :: (a -> b) -> [a] -> [b]
    map2 f x = foldr (\x acc -> f x : acc) [] x

    -- com composição
    map3 :: (a -> b) -> [a] -> [b]
    map3 f x = foldr ((:) . f) [] x