-- Javier Sagredo Tamayo. Doble Grado en Ingeniería Informática y Matemáticas. Programación Declarativa. Curso 2016-2017

import Data.List          -- Para usar las funciones permutations (l14) y intersect (l40, l41, l72)
import Data.Maybe         -- Para usar la función isJust (l59, l60, l61, l62, l85, l86)

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
ejecutarOperacion :: (Ord a, Num a) => Estado -> Palabra -> a -> a -> [Estado] -> Sol
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
comprobarNivel :: (Ord a, Num a) => Estado -> Palabra -> a -> a -> [Estado] -> Sol
comprobarNivel estado destino nivel nivelAct estadosRestantes
    | nivel > nivelAct = let a =  intersect (opReordenar destino) (snd estado) in if (not $ null a) then Just $ ("Reordenar "++(head a)):(fst estado) else ejecutarOperacion estado destino nivel (nivelAct+1) estadosRestantes
    | otherwise = Nothing

-- Función para comprobar si se encuentra la palabra destino en el estado actual. En caso contrario prosigue el proceso.
comprobarExito :: (Ord a, Num a) => Estado -> Palabra -> a -> a -> [Estado] -> Sol
comprobarExito estado destino nivel nivelAct estadosRestantes
    | elem destino (snd estado) = Just $ fst estado
    | otherwise = comprobarNivel estado destino nivel nivelAct estadosRestantes

-- Función para tomar un estado de la lista de estados acumulados, ejecutar el proceso con ese estado y, en caso de que falle, intentarlo con el siguiente estado
elegirEstado :: (Ord a, Num a) => Palabra -> a -> a -> [Estado] -> Sol
elegirEstado destino nivel nivelAct [] = Nothing
elegirEstado destino nivel nivelAct (estadoActual:estadosRestantes)
    | isJust z = z
    | isJust t = t
    | otherwise = Nothing
    where z = comprobarExito estadoActual destino nivel nivelAct estadosRestantes
          t = elegirEstado destino nivel nivelAct estadosRestantes

-- Transforma la solución mostrar bien la lista de transformaciones
muestraSol :: (Num a, Show a) => a -> Sol -> [Transformacion]
muestraSol nivel solucion = case solucion of
                              Nothing -> []
                              Just x -> reverse x

-- Función principal
solucion :: (Ord a, Num a, Show a) => [Palabra] -> Palabra -> a -> [Transformacion]
solucion palabras destino nivel = muestraSol nivel $ elegirEstado destino nivel 0 [([],palabras)]
