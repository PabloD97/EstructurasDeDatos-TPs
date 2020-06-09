-- Arboles binarios
-- Tipos algebraicos arboreos

-- ya aprendimos a definir tipos de datos nuevos, 
-- definiendo tipos algebraicos

-- Registros 
data Persona = P String Int



-- Enumerativos
data Color = Rojo | Azul
data Objeto = Espada | Escudo
data Cofre = C [Objeto]


esArma :: Objeto -> Bool
esArma Espada = True
esArma _	  = False



-- Recursivos con una parte recursiva
-- data [a] = [] | a : [a]
data Camino = Fin | Punto Cofre Camino

-- Recursivos con dos partes recursivas
data Mapa = FinMapa
	     |Bifurcacion Cofre Mapa Mapa

-- esquema para 2 tipos de recursiones
f FinMapa = ...
f (Bifurcacion cofre mapaIzq mapaDer) = 
	... f mapaIzq ... f mapaDer

existeUnArmaM :: Mapa -> Bool
existeUnArmaM FinMapa = False
existeUnArmaM (Bifurcacion cofre mapaIzq mapaDer) =
	   existeUnArmaM cofre
	|| existeUnArmaM mapaIzq 
	|| existeUnArmaM mapaDer


cantObjetosM :: Mapa -> Int
cantObjetosM FinMapa = 0
cantObjetosM (Bifurcacion cofre mapaIzq mapaDer) =
	cantObjetosM cofre +
	cantObjetosM mapaIzq +
	cantObjetosM mapaDer

-- un mapa mas complejo
ejemM1 :: Mapa
ejemM1 = Bifurcacion (C[Espada])
			(Bifurcacion (C [Escudo])
					(Bifurcacion (C [Espada])
						FinMapa
						FinMapa
					)
					(Bifurcacion (C [Escudo])
						(Bifurcacion (C [])
							FinMapa
							FinMapa
					)
					FinMapa
			)
			(Bifurcacion (C [Espada])
				FinMapa
				FinMapa
			)



-- devuelve la longitud del camino mas largo
longCamMasLargo :: Mapa -> Int
longCamMasLargo FinMapa = 0
longCamMasLargo (Bifurcacion cofre mapaIzq mapaDer)=
	1 + -- sumamos la raiz del "arbol"
	max (longCamMasLargo mapaIzq)
        (longCamMasLargo mapaDer)

objetosDelCamMasLargo :: Mapa -> [Objeto]
objetosDelCamMasLargo FinMapa =
objetosDelCamMasLargo (Bifurcacion cofre mapaIzq mapaIzq)=

	if longCamMasLargo mapaIzq >
	   longCamMasLargo mapaDer
	-- son los objetos del cam mas 
	-- largo del mapaIzq [objeto]
	-- 
		then(objetosDelCamMasLargo mapaIzq)
	
	-- son los objetos del cam mas 
	-- largo del mapaDer [objeto]
	--
		else(objetosDelCamMasLargo mapaDer)

-- definicion generica de un arbol
data Tree a =
	  EmptyT
	| NodeT a (Tree a) (Tree a) deriving Show
	 -- pueden tener 0, 1, hasta 2 nodos

-- podemos usar el generico para mapa
type Mapa = Tree Cofre 

sumT :: Tree Int -> Int
sumT EmptyT = 0
sumT (NodeT x ti td)= 
	x 
	+ (sumT ti) 
	+ (sumT td)

-- el tamaño del arbol
-- (la cantidad de nodos)
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td)=
	1 + sizeT ti + sizeT td

-- la altura es la longitud
-- del camino mas largo
heightT :: Tree a -> Int
heightT EmptyT = 0 -- para que cierre mi definicion
heightT (NodeT x ti td)=
	1 + max
	(heightT ti) 
	(heightT td)

elemsCamMasLargo :: Tree a -> [a]
elemsCamMasLargo EmptyT = []
elemsCamMasLargo (NodeT x ti td) =
	x : 
	if heightT ti > heightT td
		then elemsCamMasLargo ti -- todo un camino es una rama
		else elemsCamMasLargo td

hoja :: a -> Tree a
hoja x = NodeT x EmptyT EmptyT

-- hoja = leaf
-- hojas = leaves
-- thief el plural theives
-- es hoja cuando tiene 2 EmptyT
isEmptyT EmptyT = True
isEmptyT _		= False

leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x ti td) =
	if isEmptyT ti && isEmptyT td
		then x : leaves ti ++ leaves td
		else leaves ti ++ leaves td




