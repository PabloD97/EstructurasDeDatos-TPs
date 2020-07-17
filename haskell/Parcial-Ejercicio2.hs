-- nuevoT :: Texto — crea un texto vac´ıo.
-- letraT :: Texto -> Char -> Texto — ingresa una letra.
-- espacioT :: Texto -> Texto — ingresa un espacio
-- palabrasT :: Texto -> [String]


-- espacioT -> es el que indica donde arranca un nuevo elemento de la lista
-- si pongo varios letraT sin poner espacioT, genero un string == a la cant 
-- de letraT que puse
darVuelta :: Texto -> Texto
darVuelta txt = 
	pasarATexto nuevoT (darVueltaList (palabrasT txt))

pasarATexto :: Texto -> [String] -> Texto
pasarATexto t []     = t
pasarATexto t (s:ss) = 
	pasarATexto (agregarStringATexto t s ) ss

agregarStringATexto :: String -> Texto -> Texto
agregarStringATexto [] t 		   = espacioT t
agregarStringATexto (char:chars) t =
	letraT (agregarStringATexto t chars) char



darVueltaList :: [String] -> [String]
darVueltaList []     = []
darVueltaList (s:ss) = 
	darVueltaList ss ++ [s]

darVueltaString :: String -> String
darVueltaString [char]     = [char]
darVueltaString (char:chars) = 
	--hola -> s == h && ss == ola
	(darVueltaString chars) ++ [char]