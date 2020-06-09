import Set


data Tree a =
	  EmptyT
	| NodeT a (Tree a) (Tree a) deriving Show


-- dados una lista y un conjunto,
-- devuelve una lista
-- con todos los elementos que
-- pertenecen al conjunto
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen [] set = []
losQuePertenecen (x:xs) set =
	if belongs x set
		then x : losQuePertenecen xs set
		else losQuePertenecen xs set


-- quita todos los elementos repetidos de la lista
-- dada utilizando un conjunto como
-- estructura auxiliar
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = 
	setToList ( agregarListASet xs emptyS )

agregarListASet :: Eq a => [a] -> Set a -> Set a
agregarListASet [] set = set
agregarListASet (x:xs) set =
	agregarListASet xs (addS x set)
	

-- Dado un arbol de conjuntos
-- devuelve un conjunto con la union 
-- de todos los conjuntos del arbol
unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT = emptyS
unirTodos arbol =
	agregarListASet (unirTodosEnList arbol) emptyS 


unirTodosEnList :: Eq a => Tree (Set a) -> [a]
unirTodosEnList EmptyT = []
unirTodosEnList (NodeT a set1 set2) =
	setToList a ++
	unirTodosEnList set1 ++
	unirTodosEnList set2


addS 4 $ addS 5 $ addS 2 $ addS 4