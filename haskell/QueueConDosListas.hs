module QueueConDosListas(
			Queue,
			emptyQ,
			isEmptyQ,
			queue,
			firstQ,
			dequeue,
		)

where



data Queue a = Cola [a] [a] deriving Show

cola1 = Cola [2,3,4,5,6,1] []

-- crea una cola vacia
-- O(1)
emptyQ :: Queue a
emptyQ = Cola [] []

-- dada una cola indica
-- si la cola esta vacia
-- O(1)
isEmptyQ :: Queue a -> Bool
isEmptyQ (Cola [] _   ) = True
isEmptyQ _		        = False


-- dados un elemento y una cola
-- agrega ese elemento a la cola
-- O(1)
queue :: a -> Queue a -> Queue a
queue a (Cola fs bs) =


-- dada una cola devuelve
-- el primer elemento de la cola
-- Precondicion: la cola debe tener 
-- al menos un elemento
-- 0(1)
firstQ :: Queue a -> a
firstQ (Cola fs bs) =
	


-- dada una cola la 
-- devuelve sin su
-- primer elemento
-- O(n)
dequeue :: Queue a -> Queue a
dequeue (Cola fs bs) =
	
