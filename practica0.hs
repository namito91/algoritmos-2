module Practica0 where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True  -> "Quedate en Casa"
    False -> "Qudate en Casa"

-- b) 
casee [x]         =  []                  -- [2,4,3,2,6]  -> 4 [2,3,2,6] -> 4 3 [2,2,6]  -> 4 3 2 [2,6] 
casee (x:y:xs)    =  y : casee (x:xs)    -- 4 3 2 6 [2] -> 4 3 2 6 : [] -> [4 3 2 6] 
casee []          =  []                  

-- c)
map' f []        =  []
map' f (x:xs)    =  f x : map' f xs


-- d)
listNumeros = 1 : 2 : []


-- e) ++! -> estamos definiendo un operador
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++ ys


-- f)
addToTail x xs = xs ++ [x]




-- g)
listmin xs = head (sort xs)



-- h) (*)

prod x = x * 2

smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs




--2. Definir las siguientes funciones y determinar su tipo:

--a) five, que dado cualquier valor, devuelve 5

--b) apply, que toma una función y un valor, y devuelve el resultado de
--aplicar la función al valor dado

--c) identidad, la función identidad

--d) first, que toma un par ordenado, y devuelve su primera componente

--e) derive, que aproxima la derivada de una función dada en un punto dado
-- no es generica esta funcion, sino solo para esta funcion.

-- como realizar el desarrolo y las propiedades matematicas ??? de forma generica ?

h :: Float
h = 0.0001

x :: Float
x = 5.000

func x = (5 * x) - 3   -- 5x - 3  -->> 5(x + h) - 3 - (5x -3)

prop f x h = 5 * x + 5 * h - 3 - (5 * x) + 3 -- descomposicion matematica

-- 5 ( x + h ) - 3 - ( (5 * x) - 3 ) / h

-- 5x + 5h - 3 - 5x + 3 / h

derive :: Float -> Float -> Float

-- derive x h = prop func x h / h

derive x h = prop func x h / h
--7 + 13
--22 + 28 


-- f) sign, la función signo

sign :: Int -> Int 

sign x 
 
 | x < 0     = -1
 | x == 0    = 0
 | otherwise = 1


--g) vabs, la función valor absoluto (usando sign y sin usarla)

vabs :: Int -> Int

vabs x 
   
 | x < 0     = (-1) * x   
 | otherwise = x  

vabSign x 

 | sign x == -1 = (-1) * x
 | otherwise = x


--h) pot, que toma un entero y un número, y devuelve el resultado de
--elevar el segundo a la potencia dada por el primero

pot :: Int -> Float -> Float

pot i x = x ^ i


--i) xor, el operador de disyunción exclusiva

xor True False = True
xor False True = True
xor _ _        = False

--xor x y
 
-- | x == True && y == False = True
-- | x == False && y == True = True
-- | otherwise = False


--j) max3, que toma tres números enteros y devuelve el máximo entre ellos

compr x y 

 | x < y = y
 | x > y = x
 | otherwise = x
   
max3 :: Int -> Int -> Int -> Int

max3 x y z 

 | compr x y < z = z 
 | compr x y > z = compr x y
 | otherwise       = z

--k) swap, que toma un par y devuelve el par con sus componentes invertidas

swap :: (a,b) -> (b,a)

swap (x,y) = (y,x)


{-
3) Definir una función que determine si un año es bisiesto o no, de
acuerdo a la siguiente definición:

año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

¿Cuál es el tipo de la función definida?
-}

--4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:

--a) (Int -> Int) -> Int

igual x = x

func1 igual = igual 2

add x y = x + y

func2 f = f 1 

    
-- b) Int -> (Int -> Int)

ff :: Int -> (Int -> Int)

--ff :: Int -> Int -> Int

f x = x

ff x = f

ff' :: Int -> (Int -> Int)

ff' x = f (* 4)


--c) (Int -> Int) -> (Int -> Int)

twice f x = f (f x)

twice' f x = f x


--d) Int -> Bool

--e) Bool -> (Bool -> Bool)

--f) (Int,Char) -> Bool

--g) (Int,Int) -> Int

--h) Int -> (Int,Int)

--i) a -> Bool

--j) a -> a


--5) Definir las siguientes funciones usando listas por comprensión:

--a) 'divisors', que dado un entero positivo 'x' devuelve la
--lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)

divisors x 
 
 | x > 0     = [ i | i <- [1..x] , mod x i == 0 ]
 | otherwise = []
  
 	
--b) 'matches', que dados un entero 'x' y una lista de enteros descarta
--de la lista los elementos distintos a 'x'

matches x (ys) = [ i | i <- ys , i == x ]


--c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
--'(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
--donde 0 <= a, b, c, d <= 'n'

cuadrupla n = [ (a,b,c,d) | a <- [0..n] , b <- [0..n] , c <- [0..n] , d <- [0..n] , a^2 + b^2 == c^2 + d^2 ]


--(d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista 'xs' sin elementos repetidos
--unique :: [Int] -> [Int]

diff [] = []

diff x (y:ys) 
 
 | x /= y    = x : diff (x:xs)
 | otherwise = diff x (xs) 
 
-- nub , toma solo un elemento si hay repetidos
unique (x:xs) = [ i | i <- diff x (sort xs) ]


{-
6) El producto escalar de dos listas de enteros de igual longitud
es la suma de los productos de los elementos sucesivos (misma
posición) de ambas listas.  Definir una función 'scalarProduct' que
devuelva el producto escalar de dos listas.

Sugerencia: Usar las funciones 'zip' y 'sum'. -}

{-
7) Sin usar funciones definidas en el
preludio, defina recursivamente las siguientes funciones y
determine su tipo más general:

a) 'suma', que suma todos los elementos de una lista de números

b) 'alguno', que devuelve True si algún elemento de una
lista de valores booleanos es True, y False en caso
contrario

c) 'todos', que devuelve True si todos los elementos de
una lista de valores booleanos son True, y False en caso
contrario

d) 'codes', que dada una lista de caracteres, devuelve la
lista de sus ordinales

e) 'restos', que calcula la lista de los restos de la
división de los elementos de una lista de números dada por otro
número dado

f) 'cuadrados', que dada una lista de números, devuelva la
lista de sus cuadrados

g) 'longitudes', que dada una lista de listas, devuelve la
lista de sus longitudes

h) 'orden', que dada una lista de pares de números, devuelve
la lista de aquellos pares en los que la primera componente es
menor que el triple de la segunda

i) 'pares', que dada una lista de enteros, devuelve la lista
de los elementos pares

j) 'letras', que dada una lista de caracteres, devuelve la
lista de aquellos que son letras (minúsculas o mayúsculas)

k) 'masDe', que dada una lista de listas 'xss' y un
número 'n', devuelve la lista de aquellas listas de 'xss'
con longitud mayor que 'n' -}

{-
8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
ver su definición en https://hoogle.haskell.org/
-}
