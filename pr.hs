-- Práctica de Programación Declarativa
-- Realizada por Víctor Adolfo Gallego Alcalá

import Data.Char -- Para el toLower
import Data.Map (Map) -- Usamos esta estructura para poder contar frecuencias de elementos 
                      -- de forma más eficiente, evitando el uso de listas
import qualified Data.Map as M
 

-- Función principal
main :: IO ()
main = do
    putStr "Introduzca la ruta o nombre del archivo\n"
    nombre <- getLine
    putStr "\n\nEl texto encriptado es:\n\n\n"
    contenido <- readFile nombre
    putStr contenido
    putStr "\n\nTexto desencriptado utilizando análisis de frecuencias  carácter a carácter:\n\n\n"
    let encriptado = words (map toLower contenido)
    let clave = (zip frecCastellano (fst (unzip (qsort (M.toList ( contar frecEncriptado encriptado))))))
    desencriptado1 <- return (deco clave encriptado)
    putStr (unwords desencriptado1)
    menu desencriptado1 clave

-- Menu del programa, donde se da a elegir al usuario las distintas opciones
menu :: [[Char]] -> [(Char, Char)] -> IO ()
menu texto clave = do 
    putStr ("\n\nElija la opción que desee:\na - Sustituir un carácter por otro\nb - Pasar al analisis de frecuencias de pares de carácteres\nc - Guardar el texto en archivo\n" ++
            "d - Mostrar la clave actual\ne - Mostrar palabras más frecuentes en castellano\nf - Salir\n")
    opcion <- getChar; getChar --El segundo getChar es para consumir el Intro
    case opcion of
         'f' -> return ()
         'a' -> do  putStr "\nIntroduzca el caracter que quiere sustituir:\n"
                    char1 <- getChar; getChar
                    putStr ("\nIntroduzca el caracter por el cual quiere sustituir " ++ [char1] ++ "\n(también se sustituirá este carácter por " ++ [char1] ++ " )\n")
                    char2 <- getChar; getChar
                    nuevoTexto <- return (map (sustituye char1 char2) texto)
                    putStr (unwords nuevoTexto)
                    let nuevaClave = cambiaClave clave char1 char2 in menu nuevoTexto nuevaClave 
         'b' -> do  putStr "Introduzca el número de pares más frecuentes que quiere mostrar de ambos textos\n"
                    x <- getInt; putStr "\nTexto actual:\n"
                    print . take x . fst . unzip . qsort . M.toList $ contar2 frecEncriptado2 texto
                    putStr "\nQuijote (puede tardar un poco): "
                    lista <- inicializa contarAux2 frecQuijote2 ; putStr "\n"
                    print . take x $ lista
                    menu texto clave
         'c' -> do  putStr "Introduzca el nombre o ruta del archivo\n"
                    nombre <- getLine
                    writeFile nombre (unwords texto)
                    menu texto clave
         'd' -> do  muestraClave clave
                    menu texto clave 
         'e' -> do  putStr "Introduzca la ruta o nombre del archivo de frecuencias de palabras\n"
                    nombre <- getLine
                    archivo <-  readFile nombre
                    putStr "Introduzca la cantidad de palabras que quiere obtener (ordenadas de mayor a menor frecuencia)\n"
                    num <- getInt; putStr "Castellano:\t\t"
                    printTilde . take num . obtiene $ words archivo ; putStr "\nTexto cifrado:\t"
                    print . take num . cuentaPalabras $ texto
                    menu texto clave

-- Función auxiliar para mostrar carácteres con tilde
-- Evita el uso del show asociado al print         
printTilde :: [[Char]] -> IO ()           
printTilde [] = putStr ""        
printTilde (x:xs) = do
                    putStr (['"'] ++ x ++ ['"'] ++ ", ")
                    printTilde xs
                    
-- Función auxiliar usada para obtener las palabras del archivo de frecuencias
-- (Esta función está hecha a medida del formato del archivo) 
obtiene :: [a] -> [a]               
obtiene lista = fst. unzip. filter p $ zip lista [0..]
    where p a = if (snd a) `mod` 4 == 1 then True else False

-- Dado un texto (lista de listas de carácteres), obtiene una lista de palabras ordenadas por frecuencia
cuentaPalabras :: [[Char]] -> [[Char]]
cuentaPalabras texto = fst . unzip . qsort . M.toList $ cuentaPalabrasAux ( M.fromList $ zip texto' (repeat 0)) texto'
    where quitaPuntuacion c = if ( c < 'a' || c > 'z') then False else True
          texto' = map (filter quitaPuntuacion) texto
          cuentaPalabrasAux m t = if t == [] then m else cuentaPalabrasAux (actualiza m (head t)) (tail t)

-- Función auxiliar que lee un entero
getInt :: IO Int        
getInt = do line <- getLine
            return (read line::Int)

-- Función auxiliar que imprime por pantalla la clave actual.
-- Una clave es una lista de caracteres, que representa la biyección entre
-- el texto cifrado y lo descifrado hasta el momento
muestraClave :: [(Char, Char)] -> IO ()
muestraClave [] = return ()
muestraClave (x:xs) = do 
    putChar (fst x)
    putStr " = "
    putChar (snd x)
    putStr "\n"
    muestraClave xs

-- Dada una clave (lista de pares), intercambia en ella c1 por c2
cambiaClave :: Eq a => [(a, b)] -> a -> a -> [(a, b)]
cambiaClave [] _ _ = []
cambiaClave (x:xs) c1 c2
    | fst x == c1 = (c2, snd x):(cambiaClave xs c1 c2)
    | fst x == c2 = (c1, snd x):(cambiaClave xs c1 c2)
    | otherwise = x:(cambiaClave xs c1 c2)

-- Dada una lista, intercambia en ella c1 por c2
sustituye :: Eq a => a -> a -> [a] -> [a]
sustituye _ _ [] = []
sustituye c1 c2 (x:xs)
    | x == c1 = c2:(sustituye c1 c2 xs)
    | x == c2 = c1:(sustituye c1 c2 xs)
    | otherwise = x:(sustituye c1 c2 xs)

-- Tablas de pares carácter - frecuencia, inicializadas a 0.
frecEncriptado = M.fromList $ zip ['a' .. 'z'] (repeat 0)
frecEncriptado2 = M.fromList $ zip [ [x, y] | x <- [ 'a' .. 'z'], y <- [ 'a' .. 'z']] (repeat 0)
frecQuijote = M.fromList $ zip ['a' .. 'z'] (repeat 0)
frecQuijote2 = M.fromList $ zip [ [x, y] | x <- [ 'a' .. 'z'], y <- [ 'a' .. 'z']] (repeat 0)


-- Obtiene una lista de caracteres ordenados de mayor a menor frecuencia. 
-- f puede ser contarAux (grupos 1 caracter) o contarAux2 (grupos de 2 caracteres)
inicializa :: Ord b => (t -> [Char] -> Map a b) -> t -> IO [a]
inicializa f tablaInicial= do 
    quijote <- readFile "Quijote.txt"
    lista <- let x = map toLower (concat (words quijote)) in
         return (M.toList (f tablaInicial x))
    return (fst (unzip (qsort lista)))
    
-- Obtenida a partir de inicializa:
frecCastellano = "eaosnrildutcmpqybhvgjfzxwk"

-- Decodifica una(s) palabra(s) dada una clave
deco :: [(Char, Char)] -> [[Char]] -> [[Char]]
deco clave palabras = map (decoAux clave) palabras
decoAux ::  [(Char, Char)] -> [Char] -> [Char]
decoAux clave = map (swap clave)

-- Obtiene la pareja de a en la lista de pares dada como argumento
swap :: [(Char, Char)] -> Char -> Char
swap [] a = a
swap ((x,y):xs) a
    | y == a = x
    | otherwise = swap xs a

-- Incrementa en uno el valor asociado a la clave c dado un Map m.
-- Utilizado cuando contamos frecuencias.
actualiza :: (Ord k, Num a, Eq a) => Map k a -> k -> Map k a
actualiza m c 
    | dato == Nothing = m
    | otherwise = M.insert c ((\(Just x) -> x) dato +1) m
    where dato = M.lookup c m

-- Para contar elementos de 1 en 1
contar :: (Ord k, Num a, Eq a) => Map k a -> [[k]] -> Map k a
contar m [] = m
contar m (x:xs) = contar (contarAux m x) xs
contarAux m [] = m
contarAux m (x:xs) = contarAux (actualiza m x) xs

-- Para contar elementos de 2 en 2 (usado en la opción b del menú)
contar2 :: (Ord a1, Num a, Eq a) => Map [a1] a -> [[a1]] -> Map [a1] a
contar2 m [] = m
contar2 m (x:xs) = contar2 (contarAux2 m x) xs
contarAux2 m [] = m
contarAux2 m (x:y:xs) = contarAux2 (actualiza m (x:[y])) xs
contarAux2 m (x:xs) = m

--Ordena una lista de pares en función de la segunda componente. 
--Utilizado para ordenar las letras por frecuencia
qsort :: Ord a => [(a1, a)] -> [(a1, a)]
qsort [] = []
qsort (x:xs) =  qsort [ z | z <- xs, snd z > snd x] ++ [x] ++ qsort [ z | z <- xs, snd z <= snd x] 