
data Rubro = Almacen | Bazar Eq
type Descripcion = String
type Cantidad = Int

data ListaCompras = Vacia | Agregar Rubro Cantidad Descripcion ListaCompras

-- Eficiencia O(n)
totalDelRubro :: Map Descripcion Int -> ListaCompras -> Rubro -> Int
totalDelRubro diccPrecios Vacia rubroF 							     = 0
totalDelRubro diccPrecios (Agregar rubro cant descripcion lc) rubroF = 
	if rubro == rubroF
		then (fromJust (lookupM descripcion diccPrecios) ) 
			 + totalDelRubro diccPrecios lc rubroF
		else totalDelRubro diccPrecios lc rubroF 

fromJust :: Maybe a -> a
fromJust (Just a)   = a
