module StackCConMaximo(
			Stack,
			emptyS,
			isEmptyS,
			push,
			top,
			pop
			 )

where

-- Invariante de representacion
-- la segunda lista tiene los elementos que la 
-- primera pero ordenados de mayor a menor
data Stack a = Pila [a] [a] deriving Show


-- Crea una pila vacia
emptyS :: Stack a
emptyS = Pila [] []


-- dada una pila indica si 
-- esta vacia
isEmptyS :: Stack a -> Bool
isEmptyS (Pila [] []) = True
isEmptyS _ 		   = False

-- dados un elemento y una
-- pial, agrega el elemento
-- a la pila
push :: Ord a => a -> Stack a -> Stack a
push e (Pila elemts orden) = 
	Pila (e:elemts) (ordenar (e:orden)) 

-- dada una pila devuelve
-- el elemtno del tope de la pila
-- Precondicion = deb haber un elemtno 
top :: Stack a -> a
top (Pila a list) = head a


-- dada una pila devuelve la
-- pila sin el primer elemento
pop :: Stack a -> Stack a
pop (Pila a list) = Pila (tail a) list

maxS :: Ord a => Stack a -> a
maxS (Pila elemts orden) = maximun (elemts)

ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
	maximun xs : ordenar (sinElemento (maximun xs) xs)

maximun :: Ord a => [a] -> a
maximun [x] = x
maximun (x:xs) =
	max x (maximun xs)

-- devuelve el minimo y la lista sin el
sinElemento :: Eq a => a -> [a] -> [a]
sinElemento e [] = []
sinElemento e (x:xs) =
	if e == x 
		then sinElemento e xs
		else x : sinElemento e xs

