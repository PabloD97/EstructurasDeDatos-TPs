 
 data EscuelaDeMagia = EDM 	(Set Hechizo) (Map String Mago) (MaxHeap Mago)
-- Invariantes de representacion
-- 1) En la escuela no puede haber dos magos con el mismo nombre
-- 2) Si MaxHead esta vacia, la escuela esta vacia
-- 3) El map y la heap siempre tienen el mismo tamaño
-- 4) Cualquier hechizo que haya aprendido un mago
-- debe estar en el set



-- b) 
-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
funcarEscuela :: EscuelaDeMagia
funcarEscuela = EDM (emptyS) (emptyM) (emptyH)

-- c) 
-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM hechizos magos mPoderosos) = isEmtyH mPoderosos

-- d) 
-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: String -> EscuelaDeMagia -> EscuelaDeMagia
registrar nombre (EDM hechizos magos mPoderosos) = 
	EDM (hechizos) 
	(assocM nombre (crearM nombre) magos) 
	( insertH (crearM nombre) mPoderosos) 

-- e) 
-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM _ magos _) = domM magos

-- f) 
-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe name (EDM _ magos _) =
	hechizos ( fromJust (lookupM name magos))

fromJust :: Maybe a -> a
fromJust (Just a) = a

-- g) 
-- Propósito: Dado un mago, indica la cantidad de 
-- hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender mago (EDM hechizos magos _) =
	sizeS (hechizos) - sizeS (hechizosDe mago ((EDM hechizos magos _)))

-- h) 
-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM _ magos mPoderosos) = 
	((maxH mPoderosos) , (EDM _ (deleteM (nombre (maxH mPoderosos) magos)) (deleteMaxH mPoderosos) ) )

-- i) 
-- Propósito: Enseña un hechizo a un mago existente, 
-- y si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseñar hechizo name (EDM hechizos magos mPoderosos) =
	let mago = fromJust (lookupM name magos) in
		let magoMejorado = aprender hechizo mago
			EDM
				(addS hechizo hechizos)
				(assocM magoMejorado magos)
				(actualizar nombre magoMejorado heap)

actualizar :: String -> Mago -> Heap -> Heap
actualizar n mago heap =
	let magoMasPoderoso = maxH heap in
		if nombre magoMasPoderoso == n
			then insertH mago (deleteMaxH heap)
			else insertH magoMasPoderoso (actualizar n mago (deleteMaxH heap))

------------------------------------------------------------------------
-- Usuario

-- j) 
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos escuela = 
	setDeHechizosDeLaEscuela (magos escuela) escuela

setDeHechizosDeLaEscuela :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
setDeHechizosDeLaEscuela [] escuela = emptyS
setDeHechizosDeLaEscuela (n:ns) escuela =
	unionS (hechizosDe n escuela) (setDeHechizosDeLaEscuela ns escuela) 

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

