

data Map k v = UnMap [k] [v]

emptyM :: Map k v
emptyM = UnMap [] []

assocM :: Ord k => k -> v -> Map k v -> Map k v
assocM key value (UnMap ks vs) =
	let (keys' , values') = remplazaOAgregar key value ks vs in
		UnMap keys' values'

remplazaOAgregar :: Ord k => k -> v ->[k] -> [v] -> ([k],[v])
remplazaOAgregar key value [] _ = 
	([key],[value])
remplazaOAgregar key value (k:ks) (v:vs) =
	