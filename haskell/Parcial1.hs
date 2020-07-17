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
isEmptyRAL (MKR n dicc h) = n == 0

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

elems :: Ord a => RAList a -> [a]
elems (MKR n map heap) =
    mapToList (n-1) map

-- eficiencia O(n)
mapToList :: Int-> Map Int v -> [v]
mapToList n map =
    if n > 0
        then fromJust(lookupM n map) : (mapToList (n-1) map)
        else [fromJust(lookupM n map)]


ral = add "hola"  $ add "como" $ add "estas" $ add "bro" emptyRAL


-- h) 
-- Propósito: elimina el último elemento de la lista.
-- Precondición: la lista no está vacía.
-- Eficiencia: O(N log N).
remove :: Ord a => RAList a -> RAList a
remove (MKR n dicc h) = 
	MKR (n-1) (deleteM (n-1) dicc)
	( listToHeap ( sinElem (fromJust (lookupM (n-1) dicc) ) (heapToList h ) )
	)


listToHeap :: Ord a => [a] -> Heap a
listToHeap [] = emptyH
listToHeap (x:xs) =
	insertH x (listToHeap xs)

heapToList :: Ord a => Heap a -> [a]
heapToList h = 
	if isEmptyH h 
		then [] 
		else findMin h : heapToList (deleteMin h)

sinElem :: Ord a => a -> [a] -> [a]
sinElem a [] = []
sinElem a (x:xs) =
	if a == x 
		then xs
		else x : (sinElem a xs) 
