import Map
import Set

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

diccionario1 :: Map String Int
diccionario1 = assocM "daniel" 1 $(assocM "pepon" 3 (assocM "pepito" 5 (assocM "pepe" 56 emptyM)))

diccionario2 :: Map String Int
diccionario2 = assocM "diana" 37 $(assocM "pepon" 50 (assocM "pepito" 54 (assocM "pepita" 54 emptyM)))

diccionario3 :: Map String Int
diccionario3 = assocM "diana" 37 $(assocM "pepon" 50 (assocM "pepito" 54 (assocM "pepe" 54 emptyM)))


diccionarios :: [Map String Int]
diccionarios=[diccionario1,diccionario2,diccionario3]


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
lisToMap []     = emptyM
lisToMap (p:ps) = 
	assocM (fst p) (snd p) (lisToMap ps)


-- convierte un mapa en una
-- lista de pares clave valor
mapToList :: Eq k => Map k v -> [(k,v)]
mapToList dicc = 
	valores (domM dicc) dicc

valores :: Eq k => [k] -> Map k v -> [(k,v)]
valores [] dicc     = []
valores (k:ks) dicc = 
	(k , vMaybeJ (lookupM k dicc)) : valores ks dicc

-- tiene que existir el valor
-- de v
vMaybeJ :: Maybe v -> v
vMaybeJ (Just v) = v


-- 5)
-- Une los doms de una lista de maps.
-- En el resultado no debe haber claves repetidas.
unirDoms :: Eq k => [Map k v] -> Set k
unirDoms dics =
	agregarListASet (dooms dics) 

-- dada una lista de maps
-- devuelve todas las claves
dooms :: Eq k => [Map k v] -> [k]
dooms [] = []
dooms (dic:dics) =
	domM dic ++ dooms dics

agregarListASet :: Eq k => [k] -> Set k
agregarListASet [] = emptyS
agregarListASet (k:ks) = 
	addS k (agregarListASet ks)

-- 6)
-- Dada una lista de claves de tipo k y un mapa que 
-- va de k a int, le suma uno a cada número
-- asociado con dichas claves.
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
incrementar [] dicc = emptyM
incrementar (k:ks) dicc =
	assocM k (1+(vMaybeJ (lookupM k dicc))) (incrementar ks dicc) 
----------------
--assocM "diana" (1+(vMaybeJ (lookupM "diana" diccionario1))) (assocM "pepon" (1+(vMaybeJ (lookupM "pepon" diccionario1))) diccionario1)

-- 7)
-- Dado dos maps se agregan las claves y valores del primer
-- map en el segundo. Si una clave
-- del primero existe en el segundo,
-- es reemplazada por la del primero.
mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
mergeMaps dicc1 dicc2 = 
	unirListADicc (mapToList dicc1) dicc2

unirListADicc :: Eq k => [(k,v)] -> Map k v -> Map k v
unirListADicc [] dicc = dicc
unirListADicc (p:ps) dicc =
	assocM (fst p) (snd p) (unirListADicc ps dicc)


----- 
-- Ejercicio 3

-- dada una lista de elementos construye un map
-- que relaciona cada elemento con su posicion en la lista
indexar :: [a] -> Map Int a  
indexar []     = emptyM
indexar xs =
	assocM (length xs - 1 ) (last xs) (indexar (init xs)) 

-----------------------------------------------------------
-- Dado un string, devuelve un map donde las claves son
-- los caracteres que aparecen en el
-- string, y los valores la cantidad de veces que
-- aparecen en el mismo.
ocurrencias :: String -> Map Char Int
ocurrencias []       = emptyM
ocurrencias sts =
	assocM (last sts) (apariciones (last sts) (init sts)) (ocurrencias (init sts))

apariciones :: Char -> String -> Int
apariciones c [] = 0
apariciones c (st:sts) =
	if c == st 
		then 1 + apariciones c sts
		else apariciones c sts