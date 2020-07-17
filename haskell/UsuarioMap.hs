import Map1

-- Interfaz de Map
-- emptyM
-- assocM
-- lookupM
-- deleteM
-- domM

buscarValores :: Eq k => [k] -> Map k v -> [Maybe v]
buscarValores [] m = []
buscarValores (k:ks) m =
	lookupM k m : buscarValores ks m

valores :: Eq k => Map k v -> [v]
valores m =
	fromJusts (buscarValores (domM m) m)

-- Prec.: la lista no tiene elementos
-- Nothing
fromJusts :: [Maybe a] -> [a]
fromJusts [] = []
fromJusts (m:ms) =
	fromJust m : fromJusts ms

-- Prec.: el argumento no es Nothing
fromJust :: Maybe a -> a
fromJust (Just x) = x

ej_m =
	assocM "El tuki" 27
	(assocM "Maria"  33
	(assocM "Maria"  32
	(assocM "Carlos" 32
	 emptyM)))

