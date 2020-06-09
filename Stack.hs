module Stack(
			Stack,
			emptyS,
			isEmptyS,
			push,
			top,
			pop
			 )

where

data Stack a = Pila [a] deriving Show


-- Crea una pila vacia
emptyS :: Stack a
emptyS = Pila []


-- dada una pila indica si 
-- esta vacia
isEmptyS :: Stack a -> Bool
isEmptyS (Pila []) = True
isEmptyS _ 		   = False

-- dados un elemento y una
-- pial, agrega el elemento
-- a la pila
push :: a -> Stack a -> Stack a
push x (Pila xs) = 
	Pila (x:xs)

-- dada una pila devuelve
-- el elemtno del tope de la pila
-- Precondicion = deb haber un elemtno 
top :: Stack a -> a
top (Pila a) = head a


-- dada una pila devuelve la
-- pila sin el primer elemento
pop :: Stack a -> Stack a
pop (Pila a) = Pila (tail a)

