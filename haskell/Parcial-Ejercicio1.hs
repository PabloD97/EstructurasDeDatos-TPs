
type Nombre = String
type Fecha = Int
data Sistema = Archivo Nombre Fecha | Carpeta Nombre Sistema Sistema
type Ruta = [Nombre]

-- Eficiencia O(n)
filtrarPorFecha :: [Ruta] -> Fecha -> Sistema -> [Ruta]
filtrarPorFecha [] fecha sys = []
filtrarPorFecha rs fecha sys = 
	if rutaEsValida (head rs) sys && esFechaDeArchivo (last rs) fecha sys
		then (head rs) : filtrarPorFecha (tail rs) fecha sys 
		else filtrarPorFecha (tai rs) fecha sys

-- Eficiencia O(n)
esFechaDeArchivo :: Ruta -> Fecha -> Sistema -> Bool
esFechaDeArchivo [x] fecha (Archivo name f)	  = ( x == name) && (fecha == f) 
esFechaDeArchivo (x:xs) fecha (Carpeta name sys1 sys2) =  
	if x == name 
		then esFechaDeArchivo xs fecha sys1 || esFechaDeArchivo xs fecha sys2 
		else False


-- eficiencia O(n)
rutaEsValida :: Ruta -> Sistema -> Bool
rutaEsValida [x] (Archivo name fecha ) 			= name == x
rutaEsValida (x:xs) (Carpeta name sys1 sys2 )   = 
	if x == name 
		then rutaEsValida xs sys1 && rutaEsValida sys2
		else false


