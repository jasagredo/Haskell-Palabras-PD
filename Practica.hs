-- Javier Sagredo Tamayo. Doble Grado en Ingeniería Informática y Matemáticas. Programación Declarativa. Curso 2016-2017

-- Este programa está pensado para ser compilado y no interpretado pues en GHCi no puedes borrar de stdin un caracter y
-- como el usuario puede equivocarse en la introducción de datos, es recomendable que esto se ejecute compilado (pues ese
-- error desaparece)

import Data.List          -- Para usar las funciones permutations, intersect, intercalate
import Data.Maybe         -- Para usar la función isJust

-- Definición de tipos
type Palabra = [Char]                       -- Palabra, tipo básido de nuestro programa
type Transformacion = Palabra               -- Una transformación será codificada también como una palabra
type Sol = Maybe [Transformacion]           -- Una solución es una lista de Transformaciones o Nothing en caso de error
type Estado = ([Transformacion], [Palabra]) -- Un estado es un par de las palabras actuales y su historia (el conjunto de transformaciones que nos han llevado a ellas)

-- Operaciones con palabras
opReordenar :: Palabra -> [Palabra]
opReordenar p = [ x | x <- permutations p, x /= p ]

opConcatenar :: Palabra -> Palabra -> Palabra
opConcatenar p q = p ++ q

opIntersecar :: Palabra -> Palabra -> Palabra
opIntersecar p q = [ x | x <- p, elem x q ]

opRestar :: Palabra -> Palabra -> Palabra
opRestar p q = [x | x <- p, not $ elem x q ]

opDesplazar :: Palabra -> Palabra
opDesplazar p = [if x /= 'z' then succ x else 'a' | x <- p ]

-- Operación para obtener los pares de palabras de una lista dada
obtener2 :: [Palabra] -> [(Palabra, Palabra)]
obtener2 palabras = [ (y, z) | y <- palabras, z <- palabras, y /= z ]

-- Operaciones para quitar un/dos palabras de una lista de palabras
consumir1 :: [Palabra] -> Palabra -> [Palabra]
consumir1 palabras x = let z = break (==x) palabras in (fst z) ++ (drop 1 $ snd z)

consumir2 :: [Palabra] -> (Palabra, Palabra) -> [Palabra]
consumir2 palabras (x, y) = let z = break (==x) palabras
                                v = break (==y) palabras
                            in if length (fst z) < length (fst v) then
                               (fst z) ++ (drop 1 $ snd v) ++ intersect (fst v) (drop 1 $ snd z) else
                               (fst v) ++ (drop 1 $ snd z) ++ intersect (fst z) (drop 1 $ snd v)

-- Funciones para realizar la transformación y guardar la información de la transformación realizada
concatenar :: Estado -> [Estado]
concatenar estado = [ (("Concatenar "++x++" y "++y):fst estado,(opConcatenar x y):(consumir2 (snd estado) (x, y))) | (x,y) <- obtener2 $ snd estado ]

intersecar :: Estado -> [Estado]
intersecar estado = [ (("Intersecar "++x++" y "++y):fst estado,(opIntersecar x y):(consumir1 (snd estado) x)) | (x,y) <- obtener2 $ snd estado ]

restar :: Estado -> [Estado]
restar estado = [ (("Restar de "++x++" la palabra "++y):fst estado,(opRestar x y):(consumir1 (snd estado) x)) | (x, y) <- obtener2 $ snd estado ]

desplazar :: Estado -> [Estado]
desplazar estado = [ (("Desplazar "++x):fst estado,[  if y==x then opDesplazar y else y | y <- snd estado ]) | x <- snd estado ]

-- Función para ejecutar las distintas operaciones disponibles sobre un mismo estado
ejecutarOperacion :: Estado -> Palabra -> Int -> Int -> [Estado] -> Sol
ejecutarOperacion estado destino nivel nivelAct estadosRestantes
    | isJust c = c
    | isJust i = i
    | isJust r = r
    | isJust d = d
    | otherwise = Nothing
    where d = let z = concatenar estado in elegirEstado destino nivel nivelAct $ estadosRestantes ++ z
          i = let z = intersecar estado in elegirEstado destino nivel nivelAct $ estadosRestantes ++ z
          c = let z = restar estado in elegirEstado destino nivel nivelAct $ estadosRestantes ++ z
          r = let z = desplazar estado in elegirEstado destino nivel nivelAct $ estadosRestantes ++ z

-- Función para pasar a la siguiente fase si no hemos superado el número de pasos permitidos. Dentro de esta función se hace también la operación de comparación de las reordenaciones.
comprobarNivel :: Estado -> Palabra -> Int -> Int -> [Estado] -> Sol
comprobarNivel estado destino nivel nivelAct estadosRestantes
    | nivel > nivelAct = let a =  intersect (opReordenar destino) (snd estado) in if (not $ null a) then Just $ ("Reordenar "++(head a)):(fst estado) else ejecutarOperacion estado destino nivel (nivelAct+1) estadosRestantes
    | otherwise = Nothing

-- Función para comprobar si se encuentra la palabra destino en el estado actual. En caso contrario prosigue el proceso.
comprobarExito :: Estado -> Palabra -> Int -> Int -> [Estado] -> Sol
comprobarExito estado destino nivel nivelAct estadosRestantes
    | elem destino (snd estado) = Just $ fst estado
    | otherwise = comprobarNivel estado destino nivel nivelAct estadosRestantes

-- Función para tomar un estado de la lista de estados acumulados, ejecutar el proceso con ese estado y, en caso de que falle, intentarlo con el siguiente estado
elegirEstado :: Palabra -> Int -> Int -> [Estado] -> Sol
elegirEstado destino nivel nivelAct [] = Nothing
elegirEstado destino nivel nivelAct (estadoActual:estadosRestantes)
    | isJust z = z
    | isJust t = t
    | otherwise = Nothing
    where z = comprobarExito estadoActual destino nivel nivelAct estadosRestantes
          t = elegirEstado destino nivel nivelAct estadosRestantes

-- Transforma la solución en una lista de transformaciones
muestraSol :: Sol -> [Transformacion]
muestraSol solucion = case solucion of
                              Nothing -> ["No es posible encontrar una solución en el número de pasos proporcionado"]
                              Just x -> reverse x

-- Función principal
solucion :: [Palabra] -> Palabra -> Int -> [Transformacion]
solucion palabras destino nivel = muestraSol $ elegirEstado destino nivel 0 [([],palabras)]

-- Función principal para el cómputo sin nivel
solucion' :: [Palabra] -> Palabra -> [Transformacion]
solucion' palabras destino = muestraSol $ elegirEstado' destino [palabras] 0 [([], palabras)]

-- Funcion auxiliar para leer enteros
getInt:: IO Int
getInt = do line <- getLine
            return (read line::Int)

-- Obtener el numero de pasos que queremos permitir y realizar la llamada al proceso
intro3:: [Palabra] -> Palabra -> IO ()
intro3 p d =
       do putStrLn "Escribe el numero de pasos que quieres permitir. Por valores menores que uno se entenderá el proceso sin máximo de pasos."
          n <- getInt
          if n > 0 then putStr $ (intercalate "\n" (solucion p d n))++"\n"
                   else do putStrLn

-- Obtener la palabra de destino
intro2 :: [Palabra] -> IO ()
intro2 p =
       do putStrLn "Escribe la palabra de destino"
          n <- getLine
          intro3 p n

-- Obtener las palabras origen
intro1:: [Palabra] -> IO ()
intro1 p =
       do n <- getLine
          if n == "0" then intro2 p
                    else intro1 (n:p)


main = do putStrLn "Escribe la lista de palabras una en cada linea. Escribe 0 para continuar"
          intro1 []
