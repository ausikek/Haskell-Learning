module Aula2 where

    -- Dobrar os elementos de uma lista
    doubleList :: [Int] -> [Int]
    doubleList [] = []
    doubleList (x:xs) =  2 * x : doubleList xs

    -- Membership: se um elemento está na lista
    membership :: [Int] -> Int -> Bool
    membership [] k = False
    membership (x:xs) k
        | x == k = True
        | otherwise = membership xs k

    -- Soma de pares
    sumPairs :: [(Int, Int)] -> [Int]
    sumPairs [(x, y)] = [x+y]
    sumPairs ((a,b):xs) = (a+b) : sumPairs xs


    -- Filtrar dígitos, só retornar numéricos
    filterDigits :: String -> String
    filterDigits "" = ""
    filterDigits (c:cs)
        | c >= '0' && c <= '9' = c : filterDigits cs
        | otherwise = filterDigits cs