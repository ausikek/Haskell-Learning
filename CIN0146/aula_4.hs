module Aula4 where

    -- Consultas no banco, mas com filter map e fold

    type Pessoa = String
    type Livro = String
    type Reserva = (Pessoa, Livro)
    type Banco = [Reserva]

    -- Consultas

    livros :: Banco -> Pessoa -> [Livro]
    livros b p = map snd (filter (\(pessoa, _) -> pessoa == p) b)

    emprestimos :: Banco -> Livro -> [Pessoa]
    emprestimos b l = map fst (filter (\(_, livro) -> livro == l) b)

    emprestado :: Banco -> Livro -> Bool
    emprestado b l = length (filter (\(_, livro) -> livro == l) b) > 0

    qtdEmprestimos :: Banco -> Pessoa -> Int
    qtdEmprestimos b p = length (filter (\(pessoa, _) -> pessoa == p) b)

    -- Banco de dados de teste
    banco :: Banco
    banco =
        [ ("Ana", "Dom Casmurro")
        , ("Ana", "O Cortiço")
        , ("Bruno", "Dom Casmurro")
        , ("Carlos", "Memórias Póstumas de Brás Cubas")
        , ("Carlos", "Vidas Secas")
        , ("Carlos", "O Cortiço")
        , ("Diana", "A Hora da Estrela")
        , ("Diana", "Capitães da Areia")
        , ("Eduardo", "Vidas Secas")
        , ("Fernanda", "Dom Casmurro")
        , ("Fernanda", "A Hora da Estrela")
        , ("Gabriel", "Memórias Póstumas de Brás Cubas")
        ]
