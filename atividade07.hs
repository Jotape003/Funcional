-- IDENTIFICAÇÃO

atividade = 7

nome = "Garibaldo"

matricula = "000"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
    show (Poly []) = "0.0"
    show (Poly coeffs) =
        let
            showTerm a b=
                let
                    prefix = if a == 0 then "" else "x"
                    suffix = if a <= 1 then "" else "^" ++ show a
                    coeff' = if b == 1 && a > 0 then "" else show b
                in
                    coeff' ++ prefix ++ suffix
            showTerms = zipWith showTerm [0..] coeffs
            showTerms' = filter (not . null) showTerms
            showTerms'' = case showTerms' of
                            [] -> "0.0"
                            (t:ts) -> t ++ concatMap (\t -> "+" ++ t) ts
        in
            showTerms''


-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly coeffs) x =
    foldr (\a b -> a + b*x) 0.0 coeffs

-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0
-- data Poly = Poly [Float]


