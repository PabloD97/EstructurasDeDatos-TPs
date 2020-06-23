

-- nuevaB :: Int -> Boleteria — recibe un n´umero n y 
-- devuelve una boleter´ıa vac´ıa con n ventanillas.

-- llegarB :: Boleteria -> Boleteria — registra la 
-- llegada de una persona a la boleter´ıa. La persona se ubica
-- sobre la ventanilla que actualmente tenga menos gente esperando. 
-- En caso de empate se ubica en cualquiera de las
-- de menor longitud.

-- longitudFilaB :: NroVentanilla -> Banco -> Int — devuelve la 
-- longitud actual de la fila de gente esperando
-- en la ventanilla indicada.

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
llegarB :: Boleteria -> Boleteria
llegarB 






longitudFilaB.