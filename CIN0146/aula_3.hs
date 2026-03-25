module Aula3 where

    type Pessoa = String
    type Livro = String
    type Banco = [(Pessoa, Livro)]

    -- Consultas

    livros :: Banco -> Pessoa -> [Livro]
    livros b p = [l | (k, l) <- b, p == k]

    emprestimos :: Banco -> Livro -> [Pessoa]
    emprestimos b l = [p | (p, t) <- b, l == t]

    emprestado :: Banco -> Livro -> Bool
    emprestado b l = not (null (emprestimos b l))

    qtdEmprestimos :: Banco -> Pessoa -> Int
    qtdEmprestimos b p = length (livros b p)

    -- Mutações

    emprestar :: Banco -> Pessoa -> Livro -> Banco
    emprestar b p l = (p, l) : b

    devolver :: Banco -> Pessoa -> Livro -> Banco
    devolver b p l = [(k, t) | (k, t) <- b, p /= k && l /= t]

    -- Quicksort

    quicksort :: [Int] -> [Int]
    quicksort [] = []
    quicksort (x:xs) = quicksort m ++ [x] ++ quicksort n
        where 
            m = filter (< x)  xs
            n = filter (>= x) xs