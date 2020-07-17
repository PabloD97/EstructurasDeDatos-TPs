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

data Tree a =
	  EmptyT
	| NodeT a (Tree a) (Tree a) deriving Show

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
