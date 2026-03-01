module DayOne where
    -- Essa é outra implementaçao de allEven usando list comprehensions
   allEven :: [Integer] -> [Integer]
   allEven list = [ x | x <- list, even x]

   -- Função que recebe uma lista e retorna a lista na ordem inversa O(n)
   reverseList :: [a] -> [a]
   reverseList [] = []
   reverseList (h:t) = reverseList t ++ [h]

   -- Lista na ordem inversa, com recursão de cauda (mais otimizada)
   reverseListBetter :: [a] -> [a]
   reverseListBetter = loop []
        where 
            loop acc [] = acc
            loop acc (h:t) = loop (h:acc) t

   -- Função que retorna tuplas de cores
   colorTuples :: [[Char]] -> [([Char], [Char])]
   colorTuples [] = []
   colorTuples colors = [(colorA, colorB) | colorA <- colors, colorB <- colors, colorA /= colorB, colorA < colorB]

   -- Função que retorna uma tabela de multiplicação
   multiplicationTable :: [Integer] -> [(Integer, Integer, Integer)]
   multiplciationTable [] = []
   multiplicationTable sequence = [(numA, numB, numA * numB) | numA <- sequence, numB <- sequence]

   -- Problema map-coloring, da página 83, do capítulo de Prolog
   colors :: [[Char]]
   colors = ["red", "green", "blue"]
   
   -- Os estados são (AL, MS, GA, TN, FL)
   solutions :: [([Char], [Char], [Char], [Char], [Char])]
   solutions = 
    [
    (al, ms, ga, tn, fl) 
    | al <- colors
    , ms <- colors
    , al /= ms
    , ga <- colors
    , tn <- colors
    , al /= tn
    , al /= ga
    , fl <- colors    
    , al /= fl
    , ms /= tn
    , ga /= tn
    , ga /= fl
    ]