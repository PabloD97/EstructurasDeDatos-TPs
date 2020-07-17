module Map(Map,
			emptyM,
			assocM,
			lookupM,
			deleteM,
			domM
			)
where


data Map k v = M [(k,v)] deriving Show
-- Inv. Repre.
-- Los k son unicos
-- Cada k tiene un valor v asociado


--devuelve un map vacio
-- O(1)
emptyM :: Map k v
emptyM = M []


-- agrega un nuevo k v al map
-- si esta repetido, lo sobrescribe
-- O(n)
assocM :: Eq k => k -> v -> Map k v -> Map k v 
assocM k v (M ks) = 
	M (listDeKeys k v ks)

listDeKeys :: Eq k => k -> v -> [(k,v)] -> [(k,v)]
listDeKeys k v [] = [(k,v)]
listDeKeys k v (ks:kss) = 
	if k == fst ks 
		then (k,v) : kss
		else ks : listDeKeys k v kss

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
	M (listSinK k ks)

listSinK :: Eq k => k -> [(k,v)] -> [(k,v)]
listSinK k [] = []
listSinK k (ks:kss) = 
	if k == fst ks
		then kss
		else ks : listSinK k kss

-- dado un map k v
-- devuelve todas las keys
domM :: Map k v -> [k]
domM (M ks) = 
	allKeys ks 

allKeys :: [(k,v)] -> [k]
allKeys [] = []
allKeys (ks:kss) =
	fst ks : allKeys kss