import Map

-- obtiene los valores asociados a cada
-- clave del map
-- O(n^2) 
valuesM :: Eq k => Map k v -> [Maybe v]
valuesM mapita = 
	allValueMap (domM mapita ) mapita

allValueMap :: Eq k => [k] -> Map k v -> [Maybe v]
allValueMap [] mapita = []
allValueMap (k:ks) mapita =
	(lookupM k mapita) : allValueMap ks mapita

mapita1 = assocM "diana" 37 $(assocM "pepon" 50 (assocM "pepito" 54 (assocM "pepe" 54 emptyM)))

-- indica si en el map se encuentran
-- todas las claves dadas
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
todasAsociadas ks mapita =
	allKeysInTheMap ks (domM mapita)

allKeysInTheMap :: Eq k => [k] -> [k] -> Bool
allKeysInTheMap (k:ks) keys =
	(elem k keys) && allKeysInTheMap ks keys

-- Convierte una lista de
-- pares clave valor en un map
lisToMap :: Eq k => [(k,v)] -> Map k v
lisToMap (p:ps) = 
	assocM fst p snd p (lisToMap ps)