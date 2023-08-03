-- IDENTIFICAÇÃO
matricula = "538683" -- coloque a matricula aqui entre as aspas

-- Nome
nome = "João Pedro Soares Matias" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeiros números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [n | n <- [2..x], x `mod` n == 0] -- mude aqui

-- Determina se um números x é ou não primo
eprimo :: Int -> Bool
eprimo x = if length (divisores x) <=1 then True else False -- mude aqui

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = [x | x <- [2..n], eprimo x == True] -- mude aqui