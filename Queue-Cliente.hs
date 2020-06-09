import Queue


-- Cuenta la cantidad de elementos de la cola
-- O(1)
lengthQ :: Queue a -> Int
lengthQ cola = lenQ cola

--( queue 4 $ queue 9 $ queue 8 $ queue 2 $ queue 1 emptyQ)

-- dada una cola devuelve la lista
-- con los mismos elementos,
-- donde el orden de la lista es el 
-- de la cola
-- nota: chequear que los elementos queden
-- en el orden correcto
queueToList :: Queue a -> [a]
queueToList cola = 
	if isEmptyQ cola
		then []
		else firstQ cola : queueToList (dequeue cola)


-- Inserta todos los elementos
-- de la segunda cola en la primera
unionQ :: Queue a -> Queue a -> Queue a
unionQ cola1 cola2 =
	agregarCola2ACola1 emptyQ ((queueToList cola1) ++
	(queueToList cola2)) 

agregarCola2ACola1 :: Queue a -> [a] -> Queue a
agregarCola2ACola1 cola []     = cola
agregarCola2ACola1 cola list =
	queue (last list) (agregarCola2ACola1 cola (init list))
