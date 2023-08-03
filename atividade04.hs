-- ATIVIDADE 
atividade = 4

-- IDENTIFICAÇÃO
matricula = "538683" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Pedro Soares Matias" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Implementar função que receba uma lista
-- ou string de entrada e retorne uma outra 
-- equivalente sem repetiições de elementos,

unique :: Eq a => [a] -> [a]
unique [] = []
unique [x] = [x]
unique (x:xs) = x:unique (filter (/=x) xs) 
    -- | x `elem` xs = x:unique (filter(/=x) xs)
    -- | otherwise = x:unique xs



-- Exemplos:
-- >> unique "a1abaa1123b"
-- "a1b23"
-- >> unique [2,1,1,3,3,1,1,3,2
-- [2,1,3]]

-- Obs: (1) Note que a ordem relativa das chaves
-- remanescentes se preserva. (2) Se existir uma função em
-- Haskell que faça a mesma coisa, não deve ser usada. 


-- 2

-- Construa função que remova o valor mínimo de uma lista. 

delete'min :: (Ord a) => [a] -> [a]
delete'min [] = error "lista vazia"
delete'min [a] = [a]
delete'min (x:xs) = if x == minimum(x:xs) then xs else x: delete'min xs


-- Exemplos,

-- >> delete'min [1,3,2,5]
-- [3,2,5]
-- >> delete'min [7,3,2,5,6]
-- [7,3, 5,6]

-- Obs: (1) Se o valor mínimo se repetir
-- então somente a primeira aparição deve 
-- ser removida. (2) Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não deve se utilizada
