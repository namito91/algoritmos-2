--1. El modelo de color RGB es un modelo aditivo que tiene al rojo, verde y azul como colores
--primarios. Cualquier otro color se expresa en términos de las proporciones de estos tres colores que
--es necesario combinar en forma aditiva para obtenerlo. Dichas proporciones caracterizan a cada
--color de manera biunı́voca, por lo que usualmente se utilizan estos valores como representación de
--un color.
--Definir un tipo Color en este modelo y una función mezclar que permita obtener el promedio
--componente a componente entre dos colores

data Color = Col Float Float Float  deriving Show

mezclar :: Color -> Color -> Color

mezclar (Col x y z) (Col w e r) = (Col ((x + w) / 2) ((y + e) / 2) ((z + r) / 2))

 


------------------------------------------------------------------------------------

--2. Consideremos un editor de lı́neas simple. Supongamos que una Lı́nea es una secuencia de
--caracteres c 1 , c 2 , . . . , c n junto con una posición p, siendo 0 6 p 6 n, llamada cursor ----
--(consideraremos
--al cursor a la izquierda de un caracter que será borrado o insertado, es decir como el cursor de la
--mayorı́a de los editores). Se requieren las siguientes operaciones sobre lı́neas:

--vacı́a :: Lı́nea
--moverIzq :: Lı́nea → Lı́nea
--moverDer :: Lı́nea → Lı́nea
--moverIni :: Lı́nea → Lı́nea
--moverFin :: Lı́nea → Lı́nea
--insertar :: Char → Lı́nea → Lı́nea
--borrar :: Lı́nea → Lı́nea


--La descripción informal es la siguiente: 
--(1) la constante vacı́a denota la lı́nea vacı́a, 
--(2) la operación moverIzq mueve el cursor una posición a la izquierda (siempre que ellos sea posible), 
--(3)análogamente para moverDer , 
--(4) moverIni mueve el cursor al comienzo de la lı́nea, 
--(5) moverFin mueve el cursor al final de la lı́nea, 
--(6) la operación borrar elimina el caracterer que se encuentra a la izquierda del cursor, 
--(7) insertar agrega un caracter en el lugar donde se encontraba el cursor y lo mueve una posición a la derecha.

--Definir un tipo de datos Lı́nea e implementar las operaciones dadas.

data Linea = Ch Char Int (Linea) | EmptyC deriving Show
-- d a s 'P' t y f

r1 = Ch 'd' 0 (Ch 'a' 1 (Ch 's' 0 ( Ch 't' 0 (EmptyC)))) 

vacia :: Linea
vacia = EmptyC


moverIzq :: Linea -> Linea

moverIzq (Ch x i (Ch y 1 xs)) = Ch x 1 (Ch y 0 xs) 

moverIzq (Ch x i xs) 
 
 | i == 1    = Ch x i xs
 | otherwise = Ch x i (moverIzq xs)


moverDer :: Linea -> Linea

moverDer (Ch x 1 (EmptyC)) = (Ch x 1 (EmptyC)) 

moverDer (Ch x 1 (Ch y i ys)) = (Ch x 0 (Ch y 1 ys)) 

moverDer (Ch x i xs) = Ch x i (moverDer xs)
 

moverIni :: Linea -> Linea

helpr EmptyC = EmptyC -- otra manera de hacerlo sin esta funcion ?

helpr (Ch x i xs) = Ch x 0 ( helpr xs) 

moverIni EmptyC = EmptyC

moverIni (Ch x i xs) = Ch x 1 (helpr xs)


moverFin :: Linea -> Linea

moverFin (Ch x i EmptyC) = (Ch x 1 EmptyC)

moverFin (Ch x i xs) = Ch x 0 (moverFin xs) 


insertar :: Char -> Linea -> Linea

insertar c (Ch x i xs) 
 
 | i == 1    = Ch x 0 ( Ch c 1 xs ) 
 | otherwise = Ch x i (insertar c xs)    


borrar :: Linea -> Linea 

borrar (Ch x i xs)

 | i == 1    = Ch '-' i xs
 | otherwise = Ch x i (borrar xs)  




------------------------------------------------------------------------------------

-- 3. Dado el tipo de datos
-- data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a
-- 2 (4  (65  23) 11)  5 
-- a) Implementar las operaciones de este tipo algebraico teniendo en cuenta que:
-- Las funciones de acceso son headCL, tailCL, isEmptyCL,isCUnit.

-- headCL y tailCL no están definidos para una lista vacı́a.
-- headCL toma una CList y devuelve el primer elemento de la misma (el de más a la izquierda).
-- tailCL toma una CList y devuelve la misma sin el primer elemento.
-- isEmptyCL aplicado a una CList devuelve True si la CList es vacı́a (EmptyCL) o False
-- en caso contrario.
-- isCUnit aplicado a una CList devuelve True sii la CList tiene un solo elemento (CUnit a)
-- o False en caso contrario.

data CList a = EmptyCL | CUnit a | Consnoc a (CList a) a deriving (Show , Eq)

li1 = Consnoc 2 (Consnoc 4 (Consnoc 65 (EmptyCL) 23) 11) 5

-- li1 = Consnoc 4 (Consnoc 65 (Consnoc  (EmptyCL) 23) 11) 5


headCL :: CList a -> a

headCL EmptyCL = error "la funcion no esta definida con una lista vacia"

headCL (CUnit x) = x

headCL (Consnoc x (xs) y) = x



tailHlpr (Consnoc x EmptyCL y) i = (Consnoc x (CUnit i) y)

tailHlpr (Consnoc x (xs) y) i = (Consnoc x (tailHlpr xs i) y) 

tailCL :: CList a -> CList a

tailCL EmptyCL = error "la funcion no esta definida con una lista vacia"

tailCL (CUnit a) = EmptyCL

tailCL (Consnoc x (Consnoc w (xs) z) y) = (Consnoc w (tailHlpr xs y) z )



isEmptyCL :: CList a -> Bool

isEmptyCL EmptyCL = True

isEmptyCL (CUnit _) = False

isEmptyCL (Consnoc _ _ _) = False
	


isCUnit :: CList a -> Bool

isCUnit EmptyCL = False

isCUnit (CUnit _) = True

isCUnit (Consnoc _ _ _) = False


			
-- b) Definir una función reverseCL que toma una CList y devuelve su inversa.
-- 2 (4  (65  23) 11) 5  -->>  65 (4  (2  5) 11) 23

--       (2 5) 
--     4 (2 5) 11
-- 65 (4 (2 5) 11) 23

-- toma el 1er elem del ultimo Cosnoc
fstCL EmptyCL = error "Clist vacia"

fstCL (Consnoc x EmptyCL y) = x

fstCL (Consnoc x xs y) = fstCL xs

-- toma el 2do elem del ultimo Cosnoc
lastCL EmptyCL = error "Clist vacia"

lastCL (Consnoc x EmptyCL y) = y

lastCL (Consnoc x xs y) = lastCL xs 


-- elimina el ultimo Consnoc de la clist, para poner como nuevo "ultimo" Consnoc, el Consnoc anteultimo.
lessCL EmptyCL = EmptyCL

lessCL (Consnoc x (Consnoc w EmptyCL z) y) = (Consnoc x EmptyCL y)

lessCL (Consnoc x xs y) = (Consnoc x (lessCL xs) y)



reverseCL :: CList a -> CList a

reverseCL EmptyCL = EmptyCL

reverseCL (CUnit x) = (CUnit x) 

reverseCL (Consnoc x EmptyCL y) = (Consnoc x EmptyCL y)

reverseCL (Consnoc x xs y) = (Consnoc (fstCL xs) (reverseCL (lessCL (Consnoc x xs y))) (lastCL xs)) 




--c) Definir una función inits que toma una CList y devuelve una CList con todos los posibles
--  inicios de la CList. 

inits EmptyCL = EmptyCL

inits (Consnoc x xs y) = (Consnoc x xs y)






--d) Definir una función lasts que toma una CList y devuelve una CList con todas las posibles
--   terminaciones de la CList.

--e) Definir una función concatCL que toma una CList de CList y devuelve la CList con todas ellas
--   concatenadas
