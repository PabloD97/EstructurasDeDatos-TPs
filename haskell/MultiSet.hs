module MultiSet(

				 ) 

where

import Map

data MultiSet a = MulSet (Map a Int) deriving Show

multi =
 addMS "pera" $ addMS "manzana" $ addMS "pera" $ addMS "manzana" $ addMS "manzana" emptyMS

multi2 =
 addMS "pera" $ addMS "manzana" $ addMS "pera" $ addMS "manzana" $ addMS "manzana" emptyMS

-- denota un multiconjunto vacio
emptyMS :: MultiSet a
emptyMS = MulSet emptyM

-- Dados un elemento y un multiconjunto, 
-- agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MulSet dict ) =
	addMS' (lookupM x dict) x dict

addMS' :: Ord a => (Maybe Int) -> a -> (Map a Int) -> MultiSet a
addMS' (Nothing) x dict = MulSet (assocM x 1 dict)
addMS' (Just cantidadActual) x dict = 
	MulSet (assocM x (cantidadActual + 1) dict)


-- Dados un elemento y un multiconjunto 
-- indica la cantidad de apariciones de ese 
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS a (MulSet dict) =
	case (lookupM a dict) of 
		(Nothing) -> 0
		(Just v ) -> v


-- Dados dos multiconjuntos devuelve un 
-- multiconjunto con todos los elementos de ambos
-- multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MulSet dicc) (MulSet dicc2) =
	MulSet $ agregarTodos (domM dicc) dicc dicc2

agregarTodos :: Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
agregarTodos [] dicc dicc2 = dicc2
agregarTodos (k:ks) dicc dicc2 =
	 case (lookupM k dicc) of
	 	Nothing  -> assocM k (fromJust (lookupM k dicc )  ) (agregarTodos ks dicc dicc2) 
	 	(Just c) -> assocM k (fromJust (lookupM k dicc )+c) (agregarTodos ks dicc dicc2) 

fromJust :: Maybe a -> a
fromJust (Just v) = v



--Dado un multiconjunto devuelve una lista con 
--todos los elementos del conjunto y su cantidad
--de ocurrencias.
multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MulSet dicc) =
	armarParConOcurrencias (domM dicc) dicc 

armarParConOcurrencias :: [k] -> Map a Int -> [(a,Int)]
armarParConOcurrencias [] dicc = []
armarParConOcurrencias (k:ks) dicc = 
	(k, (fromJust (lookupM k dicc ))) :
	 armarParConOcurrencias ks dicc