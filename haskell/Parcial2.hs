
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
-- Propósito: Un organizador vacío.
-- Eficiencia: O(1)
nuevo :: Organizador
nuevo = MkO emptyM emptyM 

-- Propósito: Agrega al organizador un programa con el Checksum 
-- indicado; el conjunto es el conjunto de personas autores
-- de dicho programa.
-- Precondición: el identificador del programa que se 
-- agrega no fue usado previamente en el organizador, y el Set de personas
-- no está vacío.
-- Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
agregarPrograma MkO( dicChs dicPersonas ) chck personas =
	 MkO ( assocM chck personas dicChs )
	     ( agregarPAMap (setToList personas) chck dicPersonas )

agregarPAMap :: [Persona] -> Checksum -> Map Persona (Set Checksum) -> Map Persona (Set Checksum)
agregarPAMap [] chck map = map
agregarPAMap (p:ps) chck map = 
	assocM p ( addS chck ( fromJust (lookupM p map) ) ) (agregarPAMap ps chck map)      

fromJust :: Maybe a -> a
fromJust (Just x) = x 


setToList :: Eq a => Set a -> [a]
setToList (Conjunto a s) = a

------------------------------------------------------------------------

-- Propósito: denota una lista con todos y
-- cada uno de los códigos identificadores de programas del organizador.
-- Eficiencia: O(C) en peor caso, donde C es la cantidad
-- de códigos en el organizador.
todosLosProgramas :: Organizador -> [Checksum]
todosLosProgramas MkO( dicChs dicPersonas ) =
	domM dicChs 

----------------------------------------------------------------

-- Propósito: denota el conjunto de autores 
-- que aparecen en un programa determinado.
-- Precondición: el Checksum debe corresponder
-- a un programa del organizador.
-- Eficiencia: O(log C) en peor caso, donde C es la 
-- cantidad total de programas del organizador.
autoresDe :: Organizador -> Checksum -> Set Persona
autoresDe MkO (dicChs dicPersonas) chck = fromJust (lookupM chck dicChs)

-------------------------------------------------------------------

-- Propósito: denota el conjunto de programas en los que 
-- participó una determinada persona.
-- Precondición: la persona debe existir en el organizador.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad
-- total de personas del organizador. 
programasDe :: Organizador -> Persona -> Set Checksum
programasDe MkO( dicChs dicPersonas ) p = fromJust(lookupM p dicPersonas)

----------------------------------------------------------------------

-- Propósito: dado un organizador y dos personas, denota verdadero
-- si ambas son autores de algún software en común.
-- Precondición: las personas deben ser distintas.
-- Eficiencia: O(log P + C log C) en peor caso, donde P es 
-- la cantidad de personas distintas que aparecen en todos los
-- programas del organizador, y C la cantidad total de programas.
programaronJuntas :: Organizador -> Persona -> Persona -> Bool
programaronJuntas org p1 p2 =
	programaronJuntos (setToList (programasDe org p1 )) (programasDe org p2 )

programaronJuntos :: [Checksum] -> Set Checksum	-> Bool
programaronJuntos [] set = False 
programaronJuntos (c:cs) set =
	belongs c set || programaronJuntos cs set

----------------------------------------------------------------------

-- Propósito: dado un organizador y una persona, denota la cantidad de 
-- programas distintos en los que aparece.
-- Eficiencia: O(log P) en peor caso, donde P es la cantidad de 
-- personas del organizador.
nroProgramasDePersona :: Organizador -> Persona -> Int
nroProgramasDePersona org persona =
	sizeS (programasDe org persona) 
