module QueueDos(
			Queue,
			emptyQ,
			isEmptyQ,
			queue,
			firstQ,
			dequeue,
			lenQ
		)

where



data Queue a = Cola [a] deriving Show

cola1 = Cola [2,3,4,5,6,1]

-- crea una cola vacia
-- O(1)
emptyQ :: Queue a
emptyQ = Cola []

-- dada una cola indica
-- si la cola esta vacia
-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Cola []) = True
isEmptyQ _		   = False


-- dados un elemento y una cola
-- agrega ese elemento a la cola
-- O(1)
queue :: a -> Queue a -> Queue a
queue a (Cola es) =
	Cola (a:es)


-- dada una cola devuelve
-- el primer elemento de la cola
-- Precondicion: la cola debe tener 
-- al menos un elemento
-- 0(n)
firstQ :: Queue a -> a
firstQ (Cola a) =
	last a


-- dada una cola la 
-- devuelve sin su
-- primer elemento
-- O(n)
dequeue :: Queue a -> Queue a
dequeue (Cola elems) =
	Cola (init elems)


-- Queue con longitud constante
-- devielve la cantidad de elementos
lenQ :: Queue a -> Int -- O(1)
lenQ (Cola elems size) = size 