module Aula4 where

    -- Consultas no banco, mas com filter map e fold

    type Pessoa = String
    type Livro = String
    type BancoDados = [(Pessoa, Livro)]

    -- livros emprestados
    livros :: BancoDados -> Pessoa -> [Livro]
    livros b p = map snd (filter (\(k, l) -> p == k) b)