module Main where

import Data.List (foldl')
import Text.Read (readMaybe)

-- ============================================================
-- Practica 1 - Parte A: Paradigma Funcional (Haskell)
-- SI1002 Lenguajes y Paradigmas de Computacion - 2026-2
-- ============================================================

-- ------------------------------------------------------------
-- 1. Descomposicion aritmetica del codigo
-- ------------------------------------------------------------

-- | Ultimos tres digitos: numero consecutivo de admision.
numeroConsecutivo :: Int -> Int
numeroConsecutivo codigo = codigo `mod` 1000

-- | Dos digitos centrales: numero de la categoria (01-99).
categoriaNumero :: Int -> Int
categoriaNumero codigo = (codigo `div` 1000) `mod` 100

-- | Primeros tres digitos: periodo de admision (ej. 262).
periodo :: Int -> Int
periodo codigo = codigo `div` 100000

-- | Convierte un periodo como 262 en el anio 2026.
anioPeriodo :: Int -> Int
anioPeriodo p = 2000 + p `div` 10

-- | Obtiene el semestre del periodo.
semestre :: Int -> Int
semestre p = p `mod` 10

-- ------------------------------------------------------------
-- 2. Suma aliquota y clasificacion de Nicomaco
-- ------------------------------------------------------------

-- | Obtiene los divisores propios de un entero positivo.
--   Solo se recorren candidatos hasta n/2.
divisoresPropios :: Int -> [Int]
divisoresPropios n
    | n <= 1    = []
    | otherwise = [d | d <- [1 .. n `div` 2], n `mod` d == 0]

-- | Suma los divisores propios usando un acumulador estricto.
sumaAlicuota :: Int -> Int
sumaAlicuota n = foldl' (+) 0 (divisoresPropios n)

-- | Clasificacion de Nicomaco convertida a categoria academica.
clasificar :: Int -> String
clasificar n
    | suma > n  = "Administrative"
    | suma == n = "Engineering"
    | otherwise = "Humanities"
  where
    suma = sumaAlicuota n

-- ------------------------------------------------------------
-- 3. Paridad
-- ------------------------------------------------------------

paridad :: Int -> String
paridad codigo
    | even codigo = "even"
    | otherwise   = "odd"

-- ------------------------------------------------------------
-- 4. Validacion del codigo
-- ------------------------------------------------------------

-- | Los unicos periodos permitidos por la guia: 2026-2 a 2029-2.
periodoValido :: Int -> Bool
periodoValido p = p `elem` [262, 271, 272, 281, 282, 291, 292]

-- | Verifica la longitud, periodo, categoria y consecutivo.
codigoValido :: Int -> Bool
codigoValido codigo =
    let p    = periodo codigo
        cat  = categoriaNumero codigo
        cons = numeroConsecutivo codigo
    in codigo >= 10000000
       && codigo <= 99999999
       && periodoValido p
       && cat >= 1
       && cat <= 99
       && cons >= 1
       && cons <= 999

-- ------------------------------------------------------------
-- 5. Composicion final
-- ------------------------------------------------------------

-- | Produce exactamente las cuatro caracteristicas requeridas.
descripcion :: Int -> String
descripcion codigo =
    unwords
        [ show (anioPeriodo p) ++ "-" ++ show (semestre p)
        , clasificar (categoriaNumero codigo)
        , "num" ++ show (numeroConsecutivo codigo)
        , paridad codigo
        ]
  where
    p = periodo codigo

-- | Procesa un codigo ya convertido a Int.
procesar :: Int -> String
procesar codigo
    | codigoValido codigo = descripcion codigo
    | otherwise           = "Codigo invalido."

-- ------------------------------------------------------------
-- 6. Entrada y salida
-- ------------------------------------------------------------

bucle :: IO ()
bucle = do
    putStrLn "Ingrese el codigo (8 digitos) o escriba 'salir':"
    entrada <- getLine

    if entrada == "salir"
        then putStrLn "Programa terminado."
        else do
            case readMaybe entrada :: Maybe Int of
                Nothing     -> putStrLn "Entrada no numerica. Intente de nuevo."
                Just codigo -> putStrLn (procesar codigo)
            bucle

-- ------------------------------------------------------------
-- 7. Casos de prueba
-- ------------------------------------------------------------

casosDePrueba :: [(Int, String)]
casosDePrueba =
    [ (26276002, "2026-2 Humanities num2 even")
    , (27128112, "2027-1 Engineering num112 even")
    , (27206025, "2027-2 Engineering num25 odd")
    , (28124236, "2028-1 Administrative num236 even")
    , (28299115, "2028-2 Humanities num115 odd")
    ]

casosInvalidos :: [Int]
casosInvalidos =
    [ 26200001  -- categoria 00
    , 26201000  -- consecutivo 000
    , 26301001  -- periodo 2026-3, no permitido
    , 29301001  -- periodo 2029-3, no permitido
    , 30201001  -- fuera del rango de periodos
    , 1234567   -- menos de 8 digitos
    ]

verificar :: IO ()
verificar = do
    putStrLn "== Casos de prueba de la guia =="
    mapM_ chequearValido casosDePrueba

    putStrLn "\n== Rechazo de codigos invalidos =="
    mapM_ chequearInvalido casosInvalidos
  where
    chequearValido (codigo, esperado) =
        let obtenido = procesar codigo
            estado = if obtenido == esperado then "OK    " else "FALLO"
        in putStrLn (estado ++ show codigo ++ " -> " ++ obtenido)

    chequearInvalido codigo =
        let estado =
                if codigoValido codigo
                    then "FALLO (no fue rechazado) "
                    else "OK    rechazado "
        in putStrLn (estado ++ show codigo)

-- ------------------------------------------------------------
-- 8. Programa principal
-- ------------------------------------------------------------

main :: IO ()
main = do
    verificar
    putStrLn ""
    bucle
