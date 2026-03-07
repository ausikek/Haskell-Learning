module Aula1 where

    -- Defina uma função que compara se três números são iguais
    allEqual :: Int -> Int -> Int -> Bool
    allEqual n m p = (n == m) && (m == p)

    -- Defina uma função que, dado um valor inteiro s e um número de semanas n, retorna
    -- quantas semanas de 0 a n tiveram vendas iguais a s. Para resolver esta questão,
    -- primeiro construa uma definição simples para vendas.
    vendas :: Int -> Int
    vendas 0 = 0
    vendas x = mod x 11

    vendasIguais :: Int -> Int -> Int
    vendasIguais s 0
        | vendas 0 == s = 1
        | otherwise     = 0
    vendasIguais s n
        | vendas n == s = 1 + vendasIguais s (n-1)
        | otherwise     = vendasIguais s (n-1)

    -- Defina uma função que, dado um número inteiro, determina se ele é primo ou não
    dividesHelper :: Int -> Int -> Bool
    dividesHelper m 1 = True
    dividesHelper m n
        | mod m n == 0 = False
        | otherwise    = dividesHelper m (n-1)

    isPrime :: Int -> Bool
    isPrime m
        | m < 2     = False
        | otherwise = dividesHelper m (m-1)

    -- Defina uma função que, dados dois números inteiros x e y, determina se esses
    -- números são primos entre si
    mdc :: Int -> Int -> Int
    mdc x 0 = 0
    mdc x y = mdc y (mod x y)

    coPrimes :: Int -> Int -> Bool
    coPrimes x y
        | mdc x y == 1 = True
        | otherwise    = False

    -- Defina a função fatorial
    factorial :: Int -> Int
    factorial 0 = 1
    factorial n = n * factorial(n-1)


    -- Defina uma função que compara se quatro números são iguais
    all4Equal :: Int -> Int -> Int -> Int -> Bool
    all4Equal x y z w = (x == y) && (y == z) && (z == w)

    -- Defina a função acima usando allEqual
    all4EqualTwo :: Int -> Int -> Int -> Int -> Bool
    all4EqualTwo x y z w = allEqual x y z && allEqual y z w

    -- Defina a função que retorna quantos parâmetros são iguais
    equalCount :: Int -> Int -> Int -> Int
    equalCount x y z
        | (x == y) && (y == z) = 3
        | (x == y) || (x == z) || (y == z) = 2
        | otherwise            = 0



