import Map
import Heap2


data RAList a = MKR Int (Map Int a) (Heap a) deriving Show

-- a) 
-- Proposito: devuelve una lista vacia
-- O(1)
emptyRAL :: RAList a
emptyRAL = MKR 0 emptyM emptyH

-- b) 
-- Propósito: indica si la lista está vacía.
-- Eficiencia: O(1).
isEmptyRAL :: RAList a -> Bool
isEmptyRAL (MKR 0 dicc h) = True
isEmptyRAL (MKR n dicc h) = False

-- c) 
-- Propósito: devuelve la cantidad de elementos.
-- Eficiencia: O(1).
lengthRAL :: RAList a -> Int
lengthRAL (MKR n dicc h) = n

-- d)
-- Propósito: devuelve el elemento en el índice dado.
-- Precondición: el índice debe existir.
-- Eficiencia: O(log N).
get :: Int -> RAList a -> a
get n (MKR i dicc h) = 
	(fromJust (lookupM n dicc))

fromJust :: Maybe a -> a
fromJust (Just x) = x 


-- e) 
-- Propósito: devuelve el mínimo elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(1).
minRAL :: Ord a => RAList a -> a
minRAL (MKR n dicc h) = findMin h


-- f) 
-- Propósito: agrega un elemento al final de la lista.
-- Eficiencia: O(log N).
add :: Ord a => a -> RAList a -> RAList a
add e (MKR n dicc h) = MKR (n+1) (assocM n e dicc) (insertH e h)


-- g) 
-- Propósito: transforma una RAList en una lista, 
-- respetando el orden de los elementos.
-- Eficiencia: O(N log N). 
{-elems :: Ord a => RAList a -> [a]
elems (MKR n dicc h) =
	if n < 0
		then []
		else get n dicc : elems (MKR (n-1) (dicc) (h))   
-}

-- h) 
-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
remove (MKR n dicc h) = 
	MKR (n-1) (deleteM n dicc) h
