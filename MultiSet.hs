module MultiSet(

				 ) 

where

import Map

data MultiSet a = ElMultiSet (Map a Int) deriving Show

multi =
 addMS "pera" $ addMS "manzana" $ addMS "pera" $ addMS "manzana" $ addMS "manzana" emptyMS

multi2 =
 addMS "pera" $ addMS "manzana" $ addMS "pera" $ addMS "manzana" $ addMS "manzana" emptyMS

-- denota un multiconjunto vacio
emptyMS :: MultiSet a
emptyMS = ElMultiSet emptyM

-- Dados un elemento y un multiconjunto, 
-- agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (ElMultiSet dict ) =
	addMS' (lookupM x dict) x dict

addMS' :: Ord a => (Maybe Int) -> a -> (Map a Int) -> MultiSet a
addMS' (Nothing) x dict = ElMultiSet (assocM x 1 dict)
addMS' (Just cantidadActual) x dict = 
	ElMultiSet (assocM x (cantidadActual + 1) dict)


-- Dados un elemento y un multiconjunto 
-- indica la cantidad de apariciones de ese 
-- elemento en el multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int
ocurrencesMS a (ElMultiSet dict) =

	case (lookupM a dict) of 
		(Nothing) -> 0
		(Just v ) -> v


-- Dados dos multiconjuntos devuelve un 
-- multiconjunto con todos los elementos de ambos
-- multiconjuntos.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (ElMultiSet dict1) (ElMultiSet dict2) = 
	ElMultiSet $ agregarTodos (domM dict1) dict1 dict2
 
 agregarTodos ::Ord a => [a] -> Map a Int -> Map a Int -> Map a Int
 agregarTodos [] _ dict2 = dict2
 agregarTodos (k:ks) dict1 dict2 =
 	let cantidad = fromJust (lookupM k dict1) in 
 	let rRecursivo = agregarTodos ks dict1 dict2 in
 		case (lookupM k dict1) of 
 			Nothing	 -> assocM k cantidad rRecursivo
			(Just c) -> assocM k cantidad rRecursivo

-- dado una estructura maybe devuelve
-- el valor concreto
-- precondicion: existe un valor concreto
fromJust :: Maybe a -> a
fromJust (Just v) = v
--Dado un multiconjunto devuelve una lista con 
--todos los elementos del conjunto y su cantidad
--de ocurrencias.
-- multiSetToList :: MultiSet a -> [(a, Int)]