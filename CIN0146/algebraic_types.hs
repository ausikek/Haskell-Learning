module AlgebraicTypes where
    
    data Estacao = Inverno | Verao | Outono | Primavera

    data Temp = Quente | Frio

    clima :: Estacao -> Temp
    clima Inverno = Frio
    clima _       = Quente

    type Nome = String
    type Idade = Int

    data Pessoas = Pessoa Nome Idade
    
    -- Pessoa :: Nome -> Idade -> Pessoas, é como se fosse um construtor 

    p :: Pessoas
    p = Pessoa "José" 30

    t :: Pessoas
    t = Pessoa "Maria" 67

    mostraPessoa :: Pessoas -> String
    mostraPessoa (Pessoa n i) = n ++ " -- " ++ show i

    data Shape = Circle Float | Rectangle Float Float

    c :: Shape
    c = Circle 3.14

    r :: Shape
    r = Rectangle 6 7

    isRound :: Shape -> Bool
    isRound (Circle _) = True
    isRound (Rectangle _ _ ) = False

    -- Tipos recursivos

    data Expr = Lit Int | Add Expr Expr | Sub Expr Expr

    eval :: Expr -> Int
    eval (Lit n) = n
    eval (Add a b) = eval a + eval b
    eval (Sub a b) = eval a - eval b

    -- Polimórficos

    data Pairs t = Pair t t

    p1 :: Pairs Int
    p1 = Pair 1 2 :: Pairs Int

    p2 :: Pairs Bool
    p2 = Pair True False

    data Tree t = NilT | 
                  Node t (Tree t) (Tree t)
                  deriving (Eq, Show)