-- MÓDULOS

import System.IO
import System.Environment (getArgs)
-- não import mais nada!

--IDENTIFICAÇÃO

atividade = 8
matricula = "538683"
nome      = "João Pedro Soares Matias"

-- ATIVIDADE

-- Construir programa que leia
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa deve susturuir
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações deve ser
-- a saída. Seu nome precisa se o de f
-- o arquivo de saída deter nome igual
-- com prefixo "subst-". .

-- MATENHA O .hs COM NOME
-- "atividade.hs" E CONSEQUENTEMENTE
-- EXECUTÁVEL COMO SENDO
-- "atividade08".

-- CÓDIGO

main = do
  args <- getArgs
  let [head, w1, w2] = args

  arquivo <- readFile head
  let modarquivo = replaceAllWords w1 w2 arquivo

  let saidahead = "subst-" ++ head

  writeFile saidahead modarquivo

replaceAllWords :: String -> String -> String -> String
replaceAllWords _ _ [] = [] 
replaceAllWords antigo novo str = unlines (map replaceLine (lines str))
  where
    replaceLine line = unwords (map replaceWord (words line))
    
    replaceWord word
      | word == antigo = novo
      | word == antigo ++ "!" = novo ++ "!"
      | word == antigo ++ "," = novo ++ ","
      | word == antigo ++ ";" = novo ++ ";"
      | word == antigo ++ "." = novo ++ "."
      | word == antigo ++ "?" = novo ++ "?"
      | word == antigo ++ ":" = novo ++ ":"
      | otherwise = word


   
    

-- INFORMAÇÕES

-- Compilação e execução

-- $ ghci atividade-08.hs
-- $ ./atividade-08 historia.txt Pedro Pablo

-- Onde "historia.txt" é um arquivo de texto
-- em que toda palavra "Pedro" é substituída
-- por "Pablo".

-- Exemplo

-- "historia.txt" de entrada,

-- Pedro vivia numa casa de pedra.
-- Mas Pdro queria morar numa 
-- casa de ouro. Pobre Pedro!

-- "subst-historia.txt" criado,

-- Peblo vivia numa casa de pedra.
-- Mas Pabloqueria morar numa 
-- casa de ouro. Pobre Pablo!