data Tree a =
	  EmptyT
	| NodeT a (Tree a) (Tree a) deriving Show
	 -- pueden tener 0, 1, hasta 2 nodos




tree367 :: Tree Int
tree367 = NodeT 3 (
            NodeT 6 EmptyT EmptyT) (
            NodeT 7 EmptyT EmptyT)

tree245 :: Tree Int
tree245 = NodeT 2 (
            NodeT 4 EmptyT EmptyT) (
            NodeT 5 EmptyT EmptyT)

arbolInt :: Tree Int
arbolInt = NodeT 1 tree367 tree245

arbolStr :: Tree String
arbolStr = 
	NodeT "hi" (NodeT "you"
				EmptyT EmptyT
			) 
			(NodeT "hi"
				(NodeT "friend" EmptyT EmptyT)
				EmptyT 
			)


arbolDeListT :: Tree [Int] 
arbolDeListT = 
	NodeT [4,5] (NodeT [56,7]
				EmptyT EmptyT
			) 
			(NodeT [767,8]
				(NodeT [8789,3] EmptyT EmptyT)
				EmptyT 
			)



-- 1)
-- podemos usar el generico para mapa
sumT :: Tree Int -> Int
sumT EmptyT = 0
sumT (NodeT x ti td)= 
	x 
	+ (sumT ti) 
	+ (sumT td)

-- 2)
-- el tamaño del arbol
-- (la cantidad de nodos)
sizeT :: Tree a -> Int
sizeT EmptyT = 0
sizeT (NodeT x ti td)=
	1 + sizeT ti + sizeT td

-- 3)
-- Dado un árbol de enteros devuelve 
-- un árbol con el doble de cada número
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT = EmptyT
mapDobleT (NodeT x ti td) =
	(NodeT (x*2) (mapDobleT ti) (mapDobleT td))


-- 4)
-- Dado un árbol de palabras devuelve un 
-- árbol con la longitud de cada palabra
mapLongitudT :: Tree String -> Tree Int
mapLongitudT EmptyT = EmptyT
mapLongitudT (NodeT s ti td) =
	(NodeT (length s) (mapLongitudT ti) (mapLongitudT td) )

-- 5) 
-- Dados un elemento y un árbol binario 
-- devuelve True si existe un elemento igual a ese en el
-- árbol.
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT e EmptyT = False
perteneceT e (NodeT x ti td) =
	(e == x) || (perteneceT e ti) || (perteneceT e td)

-- 6)
-- Dados un elemento e y un árbol binario
-- devuelve la cantidad de elementos del árbol que son
-- iguales a e
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT e EmptyT = 0
aparicionesT e (NodeT x ti td) = 
	if e == x 
		then 1 + (aparicionesT e ti) + (aparicionesT e td)
		else (aparicionesT e ti) + (aparicionesT e td)

-- 7)
-- Dado un árbol devuelve su cantidad de hojas.
-- Nota: una hoja (leaf en inglés) es un nodo que no 
-- tiene hijos.
countLeaves :: Tree a -> Int
countLeaves EmptyT = 0
countLeaves (NodeT x EmptyT EmptyT) = 1
countLeaves (NodeT x ti td) =
	(countLeaves ti) + (countLeaves td)

-- 8)
-- Dado un árbol devuelve los
-- elementos que se encuentran en sus hojas.
leaves :: Tree a -> [a]
leaves EmptyT = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT x ti td) = 
	x : (leaves ti) ++ (leaves td)


-- 9)
-- Dado un árbol devuelve su altura.
-- Nota: la altura de un árbol (height en inglés), 
-- también llamada profundidad, es la cantidad
-- de niveles del árbol1
-- La altura de un árbol vacío es cero y la de una hoja es 1.
heightT :: Tree a -> Int
heightT EmptyT = 0
heightT (NodeT x ti td) =
	1 + max (heightT ti) (heightT td)

-- 10)
-- Dado un árbol devuelve el número de nodos que no son hojas.
-- ¿Cómo podría resolverla sin utilizar recursión explícita? 
-- Primero defínala con recursión explícita y después sin ella.
countNotLeaves :: Tree a -> Int
countNotLeaves EmptyT = 0
countNotLeaves (NodeT x EmptyT EmptyT) = 0 -- no leaf
countNotLeaves (NodeT x ti td) = 
	1 + (countNotLeaves ti) + (countNotLeaves td)


-- 11)
-- Dado un árbol devuelve el árbol resultante 
-- de intercambiar el hijo izquierdo con el derecho,
-- en cada nodo del árbol.
mirrorT :: Tree a -> Tree a
mirrorT (NodeT x ti td) = 
	(NodeT x td ti)

-- 12)
-- Dado un árbol devuelve una lista que representa 
-- el resultado de recorrerlo en modo in-order.
-- Nota: En el modo in-order primero se procesan 
-- los elementos del hijo izquierdo, luego la raiz
-- y luego los elementos del hijo derecho
listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NodeT x ti td) = 
	(listInOrder ti) ++ [x] ++ (listInOrder td)  

-- 13)
-- Dado un árbol devuelve una lista que representa el 
-- resultado de recorrerlo en modo pre-order.
-- Nota: En el modo pre-order primero se procesa la raiz,
-- luego los elementos del hijo izquierdo,
-- a continuación los elementos del hijo derecho.
listPreOrder :: Tree a -> [a]
listPreOrder EmptyT = []
listPreOrder (NodeT x ti td) =
	x : (listPreOrder ti) ++ (listPreOrder td)

-- 14)
-- Dado un árbol devuelve una lista que representa 
-- el resultado de recorrerlo en modo postorder.
-- Nota: En el modo post-order primero se procesan 
-- los elementos del hijo izquierdo, a
-- continuación los elementos del hijo derecho y 
-- finalmente la raiz.
listPosOrder :: Tree a -> [a]
listPosOrder EmptyT = []
listPosOrder (NodeT x ti td) =
	(listPosOrder ti) ++ 
	(listPosOrder td) ++
	[x]


-- 15)
-- Dado un árbol de listas devuelve la concatenación
-- de todas esas listas. El recorrido debe ser
-- in-order.
concatenarListasT :: Tree [a] -> [a]
concatenarListasT EmptyT = []
concatenarListasT (NodeT x ti td) =
	x ++ 
	(concatenarListasT ti) ++ 
	(concatenarListasT td)




-- 16)
levelN :: Int -> Tree a -> [a]
levelN 0 arbol = valorComoLista arbol
levelN n arbol =
	if isEmpty (arbol)
		then []
		else 
			(levelN (n-1) (ramaIzquierda arbol )) ++
			(levelN (n-1) (ramaDerecha arbol )) 
			 
-- evitar recursion entre 2 funciones
isEmpty :: Tree a -> Bool
isEmpty EmptyT = True
isEmpty _	  =  False



ramaIzquierda :: Tree a -> Tree a
ramaIzquierda (NodeT x ti td) = ti

ramaDerecha :: Tree a -> Tree a
ramaDerecha (NodeT x ti td) = td

valorComoLista :: Tree a -> [a]
valorComoLista EmptyT = []
valorComoLista (NodeT x _ _) = [x]


-- 17)
-- Dado un árbol devuelve una lista de listas
-- en la que cada elemento representa un nivel de
-- dicho árbol
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT = []
listPerLevel (NodeT x izq der) =
	[x] : (zipListas (listPerLevel izq) (listPerLevel der))

zipListas :: [[a]] -> [[a]] -> [[a]]
zipListas [] yss = yss
zipListas xss [] = xss
zipListas (xs:xss) (ys:yss) =
	(xs ++ ys) : (zipListas xss yss)
	



-- 18)
-- Devuelve los elementos de la rama más larga del árbol
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x ti td) =
	x : 
	if heightT ti > heightT td
		then ramaMasLarga ti -- todo un camino es una rama
		else ramaMasLarga td

-- 19)
-- Dado un árbol devuelve todos los caminos,
-- es decir, los caminos desde la raiz hasta las hojas.

elemtsRamaIzq :: Tree a -> [a]
elemtsRamaIzq EmptyT = []
elemtsRamaIzq (NodeT x ti td) = 
	x : elemtsRamaIzq ti ++ elemtsRamaDer td

elemtsRamaDer :: Tree a -> [a]
elemtsRamaDer EmptyT = []
elemtsRamaDer (NodeT x ti td )=
	x : elemtsRamaDer td	



-- 2 Mapa de tesoros
data Dir = Izq | Der deriving (Eq ,Show)
data Objeto = Tesoro | Chatarra deriving (Eq, Show )
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

mapa :: Mapa 
mapa =
	Bifurcacion (Cofre [Tesoro])
		(Bifurcacion (Cofre [Tesoro]) (Fin (Cofre [])) (Fin (Cofre[Tesoro])) )
		--(Bifurcacion (Cofre [Tesoro]) (Fin (Cofre [])) (Fin (Cofre [Chatarra])) )
		(Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Tesoro]))	
										(Bifurcacion (Cofre [Chatarra]) (Fin (Cofre [Tesoro])) (Fin (Cofre [])))
		)	

mapa2 :: Mapa 
mapa2 =
	Bifurcacion (Cofre [Chatarra])
		(Bifurcacion (Cofre []) (Fin (Cofre [])) (Fin (Cofre[])) )

		(Bifurcacion (Cofre [Chatarra]) (Fin (Cofre []))	
										(Bifurcacion (Cofre [Tesoro]) (Bifurcacion (Cofre [])
																		(Fin (Cofre [])) (Fin (Cofre[]))) 
										(Fin (Cofre [])))
		)

mapa3 :: Mapa
mapa3 = Bifurcacion (Cofre [])
					 ( Fin (Cofre []) ) 
					 (Bifurcacion (Cofre []) 
					 			  (Fin (Cofre[Tesoro] ))
					 			  (Fin (Cofre []))	
					 ) 		

--		(Bifurcacion Cofre []
--			Fin Cofre [Chatarra]
--			(Bifurcacion Cofre [Chatarra]
--				Fin Cofre []
--				Fin Cofre [Tesoro]
--		)
--	)
		
mmm :: Mapa
mmm = (Bifurcacion (Cofre []) (Fin (Cofre [])) (Fin (Cofre[])))
-- 1) 
-- indica si hay un tesoro en alguna parte del mapa
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c) = tieneTesoro c
hayTesoro ( Bifurcacion c mapa1 mapa2) = 
	(tieneTesoro c) ||
	(hayTesoro mapa1) ||
	(hayTesoro mapa2)

tieneTesoro :: Cofre -> Bool
tieneTesoro (Cofre []) = False
tieneTesoro (Cofre (o:os)) =
	if o == Tesoro
		then True
		else tieneTesoro (Cofre os)

-- 2)
-- Indica si al final del camino hay un tesoro.
-- Nota: el final de un camino se representa con una
-- lista vacía de direcciones
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn [] destino = hayTesoroAqui destino
hayTesoroEn (d:ds) actual =
	hayTesoroEn ds (irHacia d actual)

hayTesoroAqui :: Mapa -> Bool
hayTesoroAqui (Fin c ) = tieneTesoro c
hayTesoroAqui (Bifurcacion c _ _) =
	tieneTesoro c	

irHacia :: Dir -> Mapa -> Mapa
irHacia d (Fin c) = Fin c
irHacia Izq (Bifurcacion c izq der) = izq
irHacia Der (Bifurcacion c izq der) = der 

-- 3)
-- Indica el camino al tesoro. 
-- Precondicion: existe un tesoro y es unico
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro (Fin c) = []
caminoAlTesoro (Bifurcacion x mi mp)=
	if tieneTesoro x 
		then []
		else dirTesoro mi : caminoAlTesoro mi ++
		 caminoAlTesoro mp

dirTesoro :: Mapa -> Dir
dirTesoro arbol = 
	if tesoroEstaEnLaRamaIzq arbol
		then Izq
		else Der

tesoroEstaEnLaRamaIzq :: Mapa -> Bool
tesoroEstaEnLaRamaIzq (Fin c) = tieneTesoro c
tesoroEstaEnLaRamaIzq arbol =
	hayTesoro (mapaIzq arbol)

tesoroEstaEnLaRamaDer :: Mapa -> Bool
tesoroEstaEnLaRamaDer (Fin c) = tieneTesoro c
tesoroEstaEnLaRamaDer arbol =
	hayTesoro (mapaDer arbol)

mapaIzq :: Mapa -> Mapa
mapaIzq (Bifurcacion c izq der) = izq


mapaDer :: Mapa -> Mapa
mapaDer (Bifurcacion c izq der) = der

-- 4)
-- Indica el camino de la rama más larga.
{-
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion x (Fin c)(Fin t) )= []
caminoDeLaRamaMasLarga (Bifurcacion x mi md)=
	(dirDeLaRamaLarga mi md)  : 
	caminoDeLaRamaMasLarga mi ++ 
	caminoDeLaRamaMasLarga md

dirDeLaRamaLarga :: Mapa -> Mapa -> Dir
dirDeLaRamaLarga mi md =
	if mapMasLargo mi > mapMasLargo md
		then Izq
		else Der

mapMasLargo :: Mapa -> Int
mapMasLargo (Fin c) = 0
mapMasLargo (Bifurcacion x mi md)=
	1 + mapMasLargo mi + mapMasLargo md
-}


mapMasLargo :: Mapa  -> Int
mapMasLargo (Fin c) = 0 -- para que cierre mi definicion
mapMasLargo (Bifurcacion x ti td)=
	1 + max
	(mapMasLargo ti) 
	(mapMasLargo td)


caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin c) = []
caminoDeLaRamaMasLarga (Bifurcacion x ti td) =
	if mapMasLargo ti > mapMasLargo td
		then Izq : caminoDeLaRamaMasLarga ti -- todo un camino es una rama
		else Der : caminoDeLaRamaMasLarga td


-- 5)
-- Devuelve los tesoros separados por nivel
-- en el arbol  

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c) = [tesorosEnCofre c]
tesorosPorNivel (Bifurcacion x izq der) =
	tesorosEnCofre x : (zipListasM (tesorosPorNivel izq) (tesorosPorNivel der))

zipListasM :: [[Objeto]] -> [[Objeto]] -> [[Objeto]]
zipListasM [] yss = yss
zipListasM xss [] = xss
zipListasM (xs:xss) (ys:yss) =
	(xs ++ ys) : (zipListasM xss yss)
	

tesorosEnCofre :: Cofre -> [Objeto]
tesorosEnCofre (Cofre objs) = tesoros objs

tesoros :: [Objeto] -> [Objeto]
tesoros [] = []
tesoros (o:objs) =
	if o == Tesoro
		then o : tesoros objs
		else tesoros objs


-- Nave Espacial
data Componente = LanzaTorpedos |
				  Motor Int |
				  Almacen [Barril] deriving Show

data Barril = Comida |
			 Oxigeno | 
			 Torpedo | 
			 Combustible deriving Show

data Sector = 
	S SectorId [Componente] [Tripulante] deriving Show 

type SectorId = String
type Tripulante = String


data Nave = N (Tree Sector) deriving Show

alaIzquierda :: Sector
alaIzquierda = 
	S "alaIzquierda" [(Motor 100)] ["jose"]


alaDerecha :: Sector
alaDerecha =
	S "alaDerecha" [(Motor 100)] ["pepe","spoke"]


almcaenAlimentos :: Componente
almcaenAlimentos = Almacen [Comida]

cabina :: Sector
cabina =
	S "cabina" [almcaenAlimentos] ["jose","spoke"]



sectorAlaIzquierda :: Tree Sector
sectorAlaIzquierda = NodeT alaIzquierda EmptyT EmptyT
 
sectorAlaDerecha :: Tree Sector
sectorAlaDerecha = NodeT alaDerecha EmptyT EmptyT

sectoresEnterprise :: Tree Sector
sectoresEnterprise =
	NodeT cabina sectorAlaIzquierda sectorAlaDerecha

enterprise :: Nave 
enterprise = N sectoresEnterprise

-- 1)
-- Proposito: Devuelve todos 
-- los secotres de la nave
sectores :: Nave -> [SectorId]
sectores (N sectores) = 
	sectoresIdsT sectores

sectoresIdsT :: Tree Sector -> [SectorId]
sectoresIdsT EmptyT = []
sectoresIdsT (NodeT sector izq der) =
	(sectorId sector) :
	(sectoresIdsT izq) ++
	(sectoresIdsT der)

sectorId :: Sector -> SectorId
sectorId (S id _ _) = id

-- 2)
-- Propósito: Devuelve la suma de poder
-- de propulsión de todos los motores de la nave.
-- Nota: el poder de propulsión es el número que
-- acompaña al constructor de motores.
poderDePropulsion :: Nave -> Int
poderDePropulsion (N sectores) = 
	potenciaDeMotores (potenciaDeAlas sectores)

potenciaDeAlas :: Tree Sector -> [Componente]
potenciaDeAlas EmptyT = []
potenciaDeAlas (NodeT sector izq der) =
	soloMotores (componentes sector) ++
	potenciaDeAlas izq ++ 
	potenciaDeAlas der


esMotor :: Componente -> Bool
esMotor (Motor n) = True
esMotor _		  = False

componentes :: Sector -> [Componente]
componentes (S sector comp trip) =
	comp

soloMotores :: [Componente] -> [Componente]
soloMotores [] = []
soloMotores (c:cs) =
	if esMotor c
		then c : soloMotores cs
		else soloMotores cs

potenciaDeMotores :: [Componente] -> Int
potenciaDeMotores [] = 0
potenciaDeMotores (m:ms) =
	potencia m + potenciaDeMotores ms

potencia :: Componente -> Int
potencia (Motor n) = n

-- 3)
-- Proposito: devuelve todos los 
-- barriles de la nave
barriles :: Nave -> [Barril]
barriles (N sectores) = 
	losBarriles (barrilesEnAlmacenes sectores)

barrilesEnAlmacenes :: Tree Sector -> [Componente]
barrilesEnAlmacenes EmptyT = []
barrilesEnAlmacenes (NodeT sector izq der) =
	soloAlmacenes (componentes sector) ++
	barrilesEnAlmacenes izq ++ 
	barrilesEnAlmacenes der

-- dada una lista de componentes, devuelvo una 
-- lista compuesta por solo almacenes de tipo
-- componente
soloAlmacenes :: [Componente] -> [Componente]
soloAlmacenes [] = []
soloAlmacenes (c:cs)=
	if esAlmacen c
		then c : soloAlmacenes cs
		else soloAlmacenes cs


contDeAlmacen :: Componente -> [Barril]
contDeAlmacen (Almacen cs) = cs	

esAlmacen :: Componente -> Bool
esAlmacen (Almacen cs) = True
esAlmacen _ 		   = False	

-- dada una lista de almacenes de tipo componente
-- devuelvo una lista de barriles
losBarriles :: [Componente] -> [Barril]
losBarriles [] = []
losBarriles (a:as) =
	contDeAlmacen a ++ losBarriles as


-- 4) 
-- Propósito: Añade una lista de componentes 
-- a un sector de la nave.
-- Nota: ese sector puede no existir,
-- en cuyo caso no añade componentes.
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector c id (N sectores) = 
	if existeId id sectores
		then N (agregarAlSector c id sectores)
		else N sectores

agregarAlSector :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarAlSector c id EmptyT = EmptyT
agregarAlSector c id (NodeT sector izq der) =
	if id == sectorId sector
		then NodeT (agregarComponente c sector) izq der
		else NodeT sector (agregarAlSector c id izq) (agregarAlSector c id der)

agregarComponente :: [Componente] -> Sector -> Sector
agregarComponente c (S id comp tripu)=
	S id (c ++ comp) tripu

existeId :: SectorId -> Tree Sector -> Bool
existeId id EmptyT = False
existeId id (NodeT sector izq der) =
	(sectorId sector == id) ||
	existeId id izq ||
	existeId id der

-- 5)
-- Propósito: Incorpora un tripulante a una 
-- lista de sectores de la nave.
-- Precondición: Todos los id de la lista 
-- existen en la nave.
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
asignarTripulanteA t sid (N sectores) =
	N(agregarTASectores t sid sectores)

agregarTASectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
agregarTASectores t ids EmptyT = EmptyT
agregarTASectores t ids (NodeT sector izq der) =
	if contieneId (sectorId sector ) ids
		then NodeT (agregarT t sector) (agregarTASectores t ids izq) (agregarTASectores t ids der)
		else NodeT sector (agregarTASectores t ids izq) (agregarTASectores t ids der)
		


agregarT :: Tripulante -> Sector -> Sector
agregarT t (S id comp tripu)=
	S id comp (t:tripu)

contieneId :: SectorId -> [SectorId] -> Bool
contieneId sid [] = False
contieneId sid (id:ids) =
	if sid == id
		then True
		else contieneId sid ids

-- 6)
-- Propósito: Devuelve los sectores en donde aparece
-- un tripulante dado
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N sectores) =
	idsDeSectoresConT t sectores

idsDeSectoresConT :: Tripulante -> Tree Sector -> [SectorId]
idsDeSectoresConT t EmptyT = []
idsDeSectoresConT t (NodeT sector izq der)=
	if tieneTripulante t sector 
		then sectorId sector :
			 idsDeSectoresConT t izq ++
			 idsDeSectoresConT t der
		else idsDeSectoresConT t izq ++
			 idsDeSectoresConT t der

tieneTripulante :: Tripulante -> Sector -> Bool
tieneTripulante t (S n comp tripu)=
	contieneT t tripu

contieneT :: Tripulante -> [Tripulante] -> Bool
contieneT t [] = False
contieneT t (tri:tris) =
	if t == tri
		then True
		else contieneT t tris

-- 7)
-- Propósito: Devuelve la lista de 
-- tripulantes, sin elementos repetidos.
tripulantes :: Nave -> [Tripulante]
tripulantes (N sectores) =
	sinRepetidos (tripulantesDeSectores sectores)

tripulantesDeSectores :: Tree Sector -> [Tripulante]
tripulantesDeSectores EmptyT = []
tripulantesDeSectores (NodeT sec izq der) =
	tripulantesDeSector sec ++
	tripulantesDeSectores izq ++
	tripulantesDeSectores der

tripulantesDeSector :: Sector -> [Tripulante]	
tripulantesDeSector (S n comp tripu) = tripu

apariciones :: Eq a => a -> [a] -> Int
apariciones x [] = 0
apariciones x (y:ys) = 
	if (x == y)  
		then 1 + apariciones x ys
		else apariciones x ys

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [a] = [a]
sinRepetidos (x:xs) = 
	if apariciones x (x:xs) > 1
		then sinRepetidos xs
		else x : sinRepetidos xs



-- Manada de lobos
type Presa = String -- nombre de presa

type Territorio = String -- nombre de territorio

type Nombre = String -- nombre de lobo

data Lobo =
 Cazador Nombre [Presa] Lobo Lobo Lobo |
 Explorador Nombre [Territorio] Lobo Lobo |
 Cria Nombre deriving Show

data Manada = M Lobo deriving Show

-- 1)
manadaDeLobos :: Manada
manadaDeLobos =
	M (Cazador "malfurion" ["aves","zorros","pajarito","ave","ave"] 
		(Explorador "boton1" ["bosques","rios"] 
			(Cria "cabo1") (Cria "cabo2") )

		(Explorador "boton2" ["rios"]
			(Cria "cabo3") (Cria "cabo4") )
		(Cria "cabo5")
	   )

-- 2)
-- Propósito: dada una manada, indica si la cantidad
-- de alimento cazado es mayor a la cantidad de crias
buenCaza :: Manada -> Bool
buenCaza (M lobos) =
	alimentoCazado lobos > criasDeLobos lobos

alimentoCazado :: Lobo -> Int
alimentoCazado (Cazador n alimento l1 l2 l3) = 
	length alimento
alimentoCazado _ = 0

criasDeLobos :: Lobo -> Int
criasDeLobos (Cria n) = 1
criasDeLobos (Cazador _ _ l1 l2 l3) =
	if esCria l1
		then 1 + criasDeLobos l2 + criasDeLobos l3
		else 0 + criasDeLobos l2 + criasDeLobos l3

criasDeLobos (Explorador _ _ l1 l2) =
	if esCria l1 
		then 1 + criasDeLobos l2 
		else 0 + criasDeLobos l2


esCria :: Lobo -> Bool
esCria (Cria _) = True
esCria _ 		= False


-- 3
-- Propósito: dada una manada, devuelve el 
-- nombre del lobo con más presas cazadas, junto
-- con su cantidad de presas. 
-- Nota: se considera que los exploradores y 
-- crías tienen cero presas cazadas, y que podrían 
-- formar parte del resultado si es que no existen 
-- cazadores con más de cero presas.
{-elAlfa :: Manada -> (Nombre,Int)
elAlfa (M lobos) = ( nombreDeLobo (elQueMasCazo lobos)  , alimentoCazado (elQueMasCazo lobos) )



elQueMasCazo :: Lobo -> Lobo
elQueMasCazo (Cazador n alimt l1 l2 l3) =
	  



elMasCazador :: Lobo -> Lobo -> Lobo
elMasCazador l1 l2 =
	if (alimentoCazado l1) > (alimentoCazado l2)
		then alimentoCazado l1
		else alimentoCazado l2


nombreDeLobo :: Lobo -> Nombre
nombreDeLobo (Cazador n _ _ _ _) = n
nombreDeLobo (Explorador n _ _ _)= n
nombreDeLobo (Cria n) = n
-}

-- ejercicios de repaso. Clase de chester
cantidadDeLobosACargo :: Manada -> [(Nombre,Int)]
cantidadDeLobosACargo (M lobos) = cantidadDeLobosACargo' lobos

cantidadDeLobosACargo' :: Lobo -> [(Nombre,Int)]
cantidadDeLobosACargo' (Cria nombre) = [(nombre, 0)]

cantidadDeLobosACargo' (Explorador nombre territorio lizq lder)= 
	(nombre, 2+ aCargo lizq + aCargo lder )	: 
	cantidadDeLobosACargo' lizq ++
	cantidadDeLobosACargo' lder

cantidadDeLobosACargo' (Cazador nombre alimt lizq lcentro lder)= 
	(nombre, 3 + aCargo lizq + aCargo lcentro + aCargo lder) :
	(cantidadDeLobosACargo' lizq ++
	cantidadDeLobosACargo' lcentro ++ 
	cantidadDeLobosACargo' lder )

aCargo :: Lobo -> Int
aCargo (Cria n) = 0
aCargo (Explorador n t lizq lder) =
	2 + aCargo lizq + aCargo lder
aCargo (Cazador _ _ lizq lcentro lder) =
	3 + aCargo lizq + aCargo lcentro + aCargo lder

-------------------------------------------------------------------------------------------------------------
territoriosConocidos :: Manada -> [(Nombre, [Territorio])]
territoriosConocidos (M lobos) =
	territorioExplorado lobos

territorioExplorado :: Lobo -> [(Nombre, [Territorio])]
territorioExplorado (Cria n) = [(n,[])]
territorioExplorado (Explorador n territorio lizq lder)=
	(n , territorio ++ territorioDeLobos lizq ++ territorioDeLobos lder ) :
	territorioExplorado lizq ++
	territorioExplorado lder
territorioExplorado (Cazador n alimt lizq lcentro lder)=

	(n, territorioDeLobos lizq ++ territorioDeLobos lcentro ++ territorioDeLobos lder ) :
	territorioExplorado lizq 	++
	territorioExplorado lcentro ++
	territorioExplorado lder

territorioDeLobos :: Lobo -> [Territorio]
territorioDeLobos (Cria n) = []
territorioDeLobos (Explorador n territorio lizq lder)=
	territorio ++ territorioDeLobos lizq ++ territorioDeLobos lder


territorioDeLobos (Cazador n alimt lizq lcentro lder)=
	territorioDeLobos lizq ++ territorioDeLobos lcentro ++ territorioDeLobos lder