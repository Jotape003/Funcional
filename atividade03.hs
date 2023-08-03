-- IDENTIFICAÇÃO
matricula = "538583" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "João Pedro Soares Matias" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip xs
    -- | condicao = bloco
    | xs == [] = []
    | head xs == ' ' = strip (tail xs)
    | last xs == ' ' = strip (init xs)
    | otherwise = xs
      



-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs = let (palavra, restante) = span (/= ' ') xs in (palavra, strip restante)


-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr xs 
    | xs == [] = []
    | otherwise = palavra : splitStr restante where (palavra, restante) = popWord (strip xs)


