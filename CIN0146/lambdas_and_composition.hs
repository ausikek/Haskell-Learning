module LambdasAndComposition where

    -- dobrar lista
    doubleList :: [Int] -> [Int]
    doubleList x = map (\n -> 2 * n) x

    -- somar número
    addNumber :: Int -> Int -> Int
    addNumber n = (\m -> m + n)

    -- composição
    comp :: (a -> b) -> (c -> a) -> c -> b
    comp f g x = f (g x)