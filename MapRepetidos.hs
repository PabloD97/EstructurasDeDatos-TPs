module MapRepetidos(Map,
			emptyM,
			assocM,
			lookupM,
			deleteM,
			domM
			)
where


data Map k v = M [(k,v)] deriving Show
-- Inv. Repre.
-- Los k pueden ser repetidos
-- Cada k tiene un valor v asociado


diccionario1 :: Map String Int
diccionario1 = assocM "daniel" 1 $ assocM "pepon" 3 $ assocM "pepito" 5 $ assocM "daniel" 5 $assocM "pepe" 56 emptyM


--devuelve un map vacio
-- O(1)
emptyM :: Map k v
emptyM = M []


-- agrega un nuevo k v al map
-- si esta repetido, lo sobrescribe
-- O(1)
assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (M ks) = 
	M (ks ++ [(k,v)])

-- dado una k, devuelve el 
-- valor asociado de esta
-- O(n)
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M ks) =
	valorDeLaKey k ks 

valorDeLaKey :: Eq k => k -> [(k,v)] -> Maybe v
valorDeLaKey k [] = Nothing
valorDeLaKey k (ks:kss) =
	if k == fst ks
		then Just (snd ks)
		else valorDeLaKey k kss

-- dado una k, devuelve un map
-- sin ese par 
-- O (n)
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M ks) =
	M  (listSinK k ks)

listSinK :: Eq k => k -> [(k,v)] -> [(k,v)]
listSinK k [] = []
listSinK k (ks:kss) = 
	if k == fst ks
		then listSinK k kss
		else ks : listSinK k kss

-- dado un map k v
-- devuelve todas las keys
-- O(n)
domM ::Eq k => Map k v -> [k]
domM (M ks) = 
	sinRepetidos (allKeys ks) 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [a] = [a]
sinRepetidos (x:xs) =
	if elem x xs 
		then sinRepetidos xs
		else x : sinRepetidos xs  

allKeys :: Eq k => [(k,v)] -> [k]
allKeys [] = []
allKeys (ks:kss) =
	fst ks : allKeys kss