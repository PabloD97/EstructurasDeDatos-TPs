

-- A medida que la gente llega, se ubica en la ventanilla con menos
-- gente
type NroVentanilla = Int
data Boleteria = B  (Map NroVentanilla Int)
 -- A cada ventanilla le asocia su longitud.
					(Heap (Int, NroVentanilla))

-- Eficiencia O(log n)
nuevaB :: Int -> Boleteria
nuevaB nroVen =
	B (mapConNKeys nroVen emptyM) (heapConNPares nroVen emptyH )


-- Proposito: Creo un map con n keys
-- Eficiencia O(log n)
mapConNKeys :: Int -> Map NroVentanilla Int -> Map NroVentanilla Int 
mapConNKeys 0 diccV      = assocM 0 0 diccV
mapConNKeys nroVen diccV =
	assocM nroVen 0 (mapConNKeys (nroVen - 1) diccV)


-- Proposito: Creo una heap con n elementos pares
-- Eficiencia O(log n)
heapConNPares :: Int -> Heap a -> Heap a
heapConNPares 0 heap      = insertH (0 , 0) heap 
heapConNPares nroVen heap =
	insertH (0 , nroVen) (heapConNPares (nroVen - 1) heap)

-- Registra la llegada de una persona a la boleteria
-- .La persona se ubica sobre la ventanilla que actualmente
-- tenga menos gente esperando. En caso de empate se ubica en cualquiera de las
-- de menor longitud.
-- Eficiencia O(log m + log n)
llegarB :: Boleteria -> Boleteria
llegarB (B diccV heap) = 
	B (assocM (snd (findMin heap)) (1 + (fst(findMin heap))) diccV)
	  ( actualizarH heap )

actualizarH :: Heap (Int, NroVentanilla) -> Heap (Int, NroVentanilla)
actualizarH heap =
	insertH ((fst(findMin (heap)) + 1) (snd(findMin heap))) (deleteMin heap)


-- Devuelve la longitud actual de la fila de gente esperando
-- en la ventanilla indicada
-- Eficiencia O(log n)
longitudFilaB :: NroVentanilla -> Boleteria -> Int
longitudFilaB nroV (B diccV heap) = 
	fromJust (lookupM nroV diccV)

fromJust :: Maybe a -> a
fromJust (Just a)   = a
