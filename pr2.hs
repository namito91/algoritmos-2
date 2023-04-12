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

data Linea = Ch Char (Linea) | P (Linea)(Linea) | EmptyC deriving Show
-- d a s 'P' t y f

l1 = Ch 'd' (Ch 'a' (Ch 's' (Ch 't' (EmptyC))))
 

vacia :: Linea

vacia = EmptyC


moverIzq :: Linea -> Linea

moverIzq (Ch c xs) = Ch c EmptyC








