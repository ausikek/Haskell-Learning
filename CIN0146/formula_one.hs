module FormulaOne where
    data Piloto = Verstappen | Hamilton | Leclerc | Norris
             deriving (Show, Eq)

    type Corrida = (Piloto, Int)
    -- (Piloto, posição de chegada)

    corridas :: [Corrida]
    corridas =
        [(Verstappen,1),
        (Hamilton,2),
        (Leclerc,3),
        (Norris,4),
        (Hamilton,1),
        (Norris,2)]

    -- Defina uma função que retorna a quantidade total de pontos obtidos por um piloto.
    -- 1º lugar → 25 pontos
    -- 2º lugar → 18 pontos
    -- 3º lugar → 15 pontos
    -- qualquer outra posição → 0 pontos

    convertePonto :: Int -> Int
    convertePonto x
        | x == 1    = 25
        | x == 2    = 18
        | x == 3    = 15
        | otherwise = 0

    colocacoesPiloto :: Piloto -> [Corrida] -> [Int]
    colocacoesPiloto p c = map snd (filter (\(piloto, _) -> piloto == p) c)

    pontosPiloto :: Piloto -> [Corrida] -> Int
    pontosPiloto p c = sum (map convertePonto (colocacoesPiloto p c))