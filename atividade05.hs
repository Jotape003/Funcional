-- ATIVIDADE 
atividade = 5

-- IDENTIFICAÇÃO
matricula = "538683" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Pedro Soares Matias" -- coloque seu nome aqui entre aspas

-- 1

-- FUNÇÕES HASKELL A FAZER,

-- Construa função que 
-- receba uma string e 
-- retorne a lista das 
-- tuplas das frequencias dos
-- seus caracteres


freq :: [Char] -> [(Char, Int)]
freq str = aux str []
    where aux [] n = n
          aux (x:xs) n = aux xs (incrementaList x n)  

incrementaList :: Char -> [(Char, Int)] -> [(Char, Int)]
incrementaList c [] = [(c,1)]
incrementaList c ((x,n):xs) 
    | c == x = (x,n+1):xs
    | otherwise = (x,n) : incrementaList c xs
-- Exemplos:

-- >> freq "abcdaadd"
-- [('a',3), ('b',1),('c',1),('d',3)]
-- >> freq "A casa"
-- [('A',1), ('a', 2), ('c',1), ('s', 1), (' ',1) ]

-- Se existir uma função em
-- Haskell que faça a mesma coisa, não use.

-- 2

-- Construa função que ordene
-- a lista de tuplas da questão
-- por valor de frequencia,

freqSort :: [(Char, Int)] -> [(Char, Int)]
freqSort [] = []
freqSort ((x1, f1) : xs) =
  let less = [(x, f) | (x, f) <- xs, f < f1]
      more = [(x, f) | (x, f) <- xs, f >= f1]
  in freqSort less ++ [(x1, f1)] ++ freqSort more


-- Exemplos,

-- >> s = freqSort freq "aaaa22p"
-- [('p',1), ('2', 2), ('a', 4)]

-- Obs: Se existir uma função 
-- em Haskell que faça a mesma coisa, 
-- não use