module SetRepetidos( Set,
		    emptyS,
		    addS,
		    belongs,
		    sizeS,
		    removeS,
		    unionS,
		    intersectionS,
		    setToList
		    )

where 

-- 2. Set (conjunto)
data Set a = Conjunto [a] Int deriving Show

set1 = Conjunto [1,2,3,4,5]

set2 = Conjunto [1,2,3,8,9,302]


-- crea un conjunto vacio
emptyS :: Set a 
emptyS = Conjunto [] 0

-- dados un elemento y un conjunto, agrega el
-- agrega el elemento al conjunto
addS :: Eq a => a -> Set a -> Set a
addS a (Conjunto xs size) = 
	Conjunto (a:xs) (size+1)


-- dados un elemento y un conjunto indica si el
-- elemento pertenece al conjunto
belongs :: Eq a => a -> Set a -> Bool
belongs e (Conjunto elems size ) = elem e elems

-- devuelve la cantidad de elementos
-- distintos de un conjunto
sizeS :: Eq a => Set a -> Int
sizeS (Conjunto elems size) = 
	length (sinRepetidos elems)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [a] = [a]
sinRepetidos (x:xs) = 
	if elem x xs
		then sinRepetidos xs
		else x : sinRepetidos xs


-- borra un elemento del conjunto
removeS :: Eq a => a -> Set a -> Set a
removeS a (Conjunto elems size) = 
	if elem a elems
		then Conjunto (remover a elems) (size -1)
		else Conjunto elems size
	
remover :: Eq a => a -> [a] -> [a]
remover a (x:xs)= 
	if a == x
		then xs
		else x : remover a xs 

-- dados dos conjuntos devuelve un conjunto
-- con todos los elementos de ambos conjuntos
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Conjunto xs size2) (Conjunto ys size) =
	Conjunto (unionSinRepetidos xs ys) 
	(length(unionSinRepetidos xs ys) )

unionSinRepetidos :: Eq a => [a] -> [a] -> [a]
unionSinRepetidos [] ys     = ys 
unionSinRepetidos (x:xs) ys = 
	if elem x ys
		then unionSinRepetidos xs ys 
		else x : unionSinRepetidos xs ys 


-- dados dos conjuntos devuelve
-- un conjunto con todos los elementos
-- en comun entre ambos
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (Conjunto xs s1 ) (Conjunto ys s2) =
	Conjunto (intersectionSinRepetidos xs ys)
	(length (intersectionSinRepetidos xs ys))

intersectionSinRepetidos :: Eq a => [a] -> [a] -> [a]
intersectionSinRepetidos [] ys = []
intersectionSinRepetidos (x:xs) ys =
	if elem x ys
		then x:intersectionSinRepetidos xs ys
		else intersectionSinRepetidos xs ys

-- dado un conjunto devuelve una lista con todos
-- los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]
setToList (Conjunto a s) = 
	sinRepetidos a