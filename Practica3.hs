


-- 1)
data Persona = ConstP String Int deriving Show

{-pepe  = ConstP "pepe" 23
jorge = ConstP "jorge" 19
betoben = ConstP "betoben" 12
raulsito = ConstP "raulsito" 40
-}
-- Devuelve el nombre de una persona 
nombre :: Persona -> String
nombre (ConstP name age) = name

edad :: Persona -> Int
edad (ConstP name age) = age

crecer :: Persona -> Persona
crecer (ConstP name age) = ConstP name (age+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre string (ConstP name age) = 
	ConstP string age

esMenorQueLaOtra :: Persona -> Persona -> Bool
esMenorQueLaOtra (ConstP name age) (ConstP n a) =
	age < a

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n [] = []
mayoresA n (x:xs) =
	if  n < edad x
		then x : mayoresA n xs 
		else mayoresA n xs

totalEdades :: [Persona] -> Int
totalEdades [] = 0
totalEdades (x:xs) = edad x + totalEdades xs

promedioEdad :: [Persona] -> Int
promedioEdad xs = div (totalEdades xs) (length xs)

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps) = edad p : edades ps

maximun :: Ord a => [a] -> a
maximun [x] = x
maximun (x:xs) =
	max x (maximun xs)

elMasViejo :: [Persona] -> Persona
elMasViejo [x] = x
elMasViejo (p:ps) = 
	if maximun (edades (p:ps)) == edad p
		then p
		else elMasViejo ps

-- 2)
data TipoDePokemon= Agua |
				   Fuego | 
				   Planta deriving (Show , Eq)

type Energia = Int
data Pokemon = Poke TipoDePokemon Energia deriving Show

data Entrenador = Entre String [Pokemon] deriving Show

agumon = Poke Fuego 100
gabumon = Poke Agua 90
plantamon = Poke Planta 160
guilmon = Poke Fuego 200

tay = Entre "tay" [agumon,gabumon,plantamon, guilmon]
ash = Entre "ash" [agumon,gabumon]


superaPoderA :: TipoDePokemon -> TipoDePokemon -> Bool
superaPoderA Agua Fuego = True
superaPoderA Fuego Planta= True
superaPoderA Planta Agua = True
superaPoderA _ _  = False


superaA :: Pokemon -> Pokemon -> Bool
superaA (Poke tipo1 _) (Poke tipo2 _)=
	superaPoderA tipo1 tipo2



pokemones :: Entrenador -> [Pokemon]
pokemones (Entre name ps) = ps

cantPokemones :: Entrenador -> Int
cantPokemones (Entre name ps) =
	length (pokemones (Entre name ps))

tipoPoke :: Pokemon -> TipoDePokemon
tipoPoke (Poke tipo energia) = tipo 

cantPokemonesDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonesDe t (Entre n []) = 0
cantPokemonesDe t (Entre n pokemones)=
	cantPokemonesDe' t pokemones


esMismoTipo :: TipoDePokemon -> TipoDePokemon -> Bool
esMismoTipo Planta Planta = True
esMismoTipo Agua Agua = True
esMismoTipo Fuego Fuego = True
esMismoTipo _ _ = False

esTipoPokemon :: TipoDePokemon -> Pokemon -> Bool
esTipoPokemon tipoBuscado (Poke tipo _) =
	esMismoTipo tipoBuscado tipo

cantPokemonesDe' :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonesDe' tipoBuscado [] = 0
cantPokemonesDe' tipoBuscado (t:ts) = 
	(if esTipoPokemon tipoBuscado t then 1 else 0)



concatPokemon :: [Entrenador] -> [Pokemon]
concatPokemon [] = []
concatPokemon (x:xs) =
	pokemones x ++ concatPokemon xs


esMaestro :: Entrenador -> Bool
esMaestro (Entre n (p:ps)) =
	if cantPokemonesDe Agua (Entre n (p:ps)) >= 1
	&& cantPokemonesDe Planta (Entre n (p:ps)) >= 1
	&& cantPokemonesDe Planta (Entre n (p:ps)) >= 1
		then True
		else False


-- 3)
data Color = Azul | Rojo deriving Show
data Celda = ConsCelda [Color] deriving Show


celdaVacia :: Celda
celdaVacia = ConsCelda []

esElMismoColor :: Color -> Color -> Bool
esElMismoColor Azul Azul = True
esElMismoColor Rojo Rojo = True
esElMismoColor _ _ = False


nroBolitas :: Color -> Celda -> Int
nroBolitas color (ConsCelda []) = 0
nroBolitas color (ConsCelda cs) =
	sumarSiEsElMismo color cs

sumarSiEsElMismo :: Color -> [Color] -> Int
sumarSiEsElMismo color [] = 0
sumarSiEsElMismo color (c:cs) =
	if esElMismoColor color c 
		then 1 + sumarSiEsElMismo color cs
		else 0 + sumarSiEsElMismo color cs




poner :: Color -> Celda -> Celda
poner color (ConsCelda []) = (ConsCelda [color])
poner color (ConsCelda xs) = (ConsCelda (color:xs))

sacar :: Color -> Celda -> Celda
sacar color (ConsCelda []) = (ConsCelda [])
sacar color (ConsCelda cs) = sacarColor color cs
	
sacarColor :: Color -> [Color] -> Celda 
sacarColor color cs  =
	(ConsCelda (sinColor color cs))

sinColor :: Color -> [Color] -> [Color]
sinColor color (c:cs) = 
	if esElMismoColor color c
		then cs
		else c : sinColor color cs


ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 color (ConsCelda cs) = (ConsCelda cs)
ponerN n color (ConsCelda cs) = 
	if n > 0
		then ponerN (n-1) color (ConsCelda (color:cs)) 
		else (ConsCelda cs)

-- 4)
type Proyecto = String
data Senority = Junior | SemiSenior | Senior deriving Show
data Rol = Desarrollo Senority Proyecto |
		   Management Senority Proyecto deriving Show
data Empresa = ConstEmpresa [Rol]

rol1 = Desarrollo Junior "progresar"
rol2 = Management SemiSenior "algoritmos"
rol3 = Desarrollo Senior "progresar"

imb = ConstEmpresa [rol1,rol2,rol3, rol1,rol3]

proyectoDeRol :: Rol -> Proyecto
proyectoDeRol  (Desarrollo _ proyecto) = proyecto
proyectoDeRol  (Management _ proyecto) = proyecto

proyectosDeRoles :: [Rol] -> [Proyecto]
proyectosDeRoles [] = []
proyectosDeRoles (r:rs) =
	proyectoDeRol r : proyectosDeRoles rs


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

proyectos :: Empresa -> [Proyecto]
proyectos (ConstEmpresa ps) = 
	sinRepetidos (proyectosDeRoles ps)


losDevSenior :: Empresa -> Int
losDevSenior (ConstEmpresa rs) = 
	cantDeDesarrolladoresSenior rs

cantDeDesarrolladoresSenior :: [Rol] -> Int
cantDeDesarrolladoresSenior [] = 0
cantDeDesarrolladoresSenior (r:rs) =
	if esDesarrollador r && esSeniorDesarrollador r
		then 1 + cantDeDesarrolladoresSenior rs
		else 0 + cantDeDesarrolladoresSenior rs


esDesarrollador :: Rol -> Bool
esDesarrollador (Desarrollo _ _) = True
esDesarrollador (Management _ _) = False

esSeniorDesarrollador :: Rol -> Bool
esSeniorDesarrollador (Desarrollo Senior _) = True
esSeniorDesarrollador (Desarrollo _ _ ) = False
esSeniorDesarrollador (Management _ _) = False


trabajanEnEsteProyecto :: Proyecto -> [Rol] -> Int
trabajanEnEsteProyecto p [ ] = 0
trabajanEnEsteProyecto proyecto (r:rs) =
	if proyecto == proyectoDeRol r
		then 1 + trabajanEnEsteProyecto proyecto rs
		else 0 + trabajanEnEsteProyecto proyecto rs

losQueTrabajanEn :: Proyecto -> Empresa -> Int
losQueTrabajanEn proyecto (ConstEmpresa rs) =
	trabajanEnEsteProyecto proyecto rs









asignadosPorProyectos :: Empresa -> [(Proyecto,Int)]
asignadosPorProyectos (ConstEmpresa rs)=
	paresDeProyectos 
		(sinRepetidos (proyectosDeRoles rs))
		rs


paresDeProyectos :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
paresDeProyectos [] rs = []
paresDeProyectos ps [] = [] 
paresDeProyectos (p:ps) rs =
	parProyecto p rs : paresDeProyectos ps rs


parProyecto :: Proyecto -> [Rol] -> (Proyecto, Int)
parProyecto proyecto rs =
	(proyecto, trabajanEnEsteProyecto proyecto rs )
        

-- 5) Secuencia de celdas

data SecuenciaDeCeldas = 
	Sec [Celda] Celda [Celda] deriving Show

data Dir = Izq | Der

celda1 = ConsCelda [Rojo, Azul]
celda2 = ConsCelda [Rojo, Rojo]
celda3 = celdaVacia
celda4 = ConsCelda [Azul, Rojo, Azul]
celda5 = ConsCelda [Rojo, Azul, Rojo]
celda6 = ConsCelda [Azul, Azul]

secuencia1 =
	Sec [celda1, celda2] celda3 [celda4, celda5, celda6] 

secuencia2 = Sec [] celda1 [celda2,celda3]

snoc :: [a] -> a -> [a]
snoc [] e =  [e]
snoc (x:xs) e = 
	x : (snoc xs e)


celdas :: SecuenciaDeCeldas -> [Celda]
celdas (Sec izq actual derecha) =
	(snoc izq actual) ++ derecha

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty xs = False

enElOrigen :: SecuenciaDeCeldas -> Bool
enElOrigen (Sec izq actual derecha) =
	isEmpty izq



takeN :: Int -> [a] -> [a]
takeN 0 (x:xs) = [] 
takeN n [] = []
takeN n (x:xs) = 
	if n > length (x:xs)
		then []
		else x : takeN (n-1) xs


dropN :: Int -> [a] -> [a]
dropN 0 [] = []
dropN 0 (x:xs) = (x:xs)
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

elementoN :: Int -> [a] -> a
elementoN n (x:xs) =
	if n > 0
		then elementoN (n-1) xs
		else x 


nuevaSecuencia :: Int -> [Celda] -> SecuenciaDeCeldas
nuevaSecuencia n cs =
	Sec (takeN (n-1) cs)  
		(elementoN (n-1) cs) 
		(dropN n cs) 	

dameElUltimo :: [a] -> a -- devuelve el ultimo elemento de la lista
dameElUltimo xs = 
	elementoN ((length xs) -1) xs

eliminarUltimo :: [a] -> [a] -- deuvelve la lista sin el ultimo elemento
eliminarUltimo [] = []
eliminarUltimo xs =
	takeN ((length xs) - 1) xs


mover :: Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas
mover Izq (Sec [] actual der)  = Sec [] actual der
mover Izq (Sec izq actual der) = Sec (eliminarUltimo izq) (dameElUltimo izq) (actual:der)
mover Der (Sec izq actual [])  = Sec izq actual []
mover Der (Sec izq actual der) = Sec (snoc izq actual) (head der) (tail der) 



moverN :: Int -> Dir -> SecuenciaDeCeldas -> SecuenciaDeCeldas
moverN 0 dir (Sec izq actual der) = Sec izq actual der 
moverN n dir (Sec izq actual der) = 
	moverN (n-1) dir (mover dir (Sec izq actual der))

irAlOrigen :: SecuenciaDeCeldas -> SecuenciaDeCeldas
irAlOrigen (Sec izq actual der) =
	Sec [] (head izq) ( tail izq ++ (actual: der) )


totalDeBolitas :: SecuenciaDeCeldas -> Int
totalDeBolitas (Sec izq actual der) = 
	nroBolitasTotalesEnCeldas izq + 
	nroBolitasTotales actual +
	nroBolitasTotalesEnCeldas der



nroBolitasTotalesEnCeldas :: [Celda] -> Int
nroBolitasTotalesEnCeldas [] = 0
nroBolitasTotalesEnCeldas (c:cs) =
	nroBolitasTotales c + nroBolitasTotalesEnCeldas cs

nroBolitasTotales :: Celda -> Int
nroBolitasTotales (ConsCelda bs) =
	nroBolitasTotal bs 

nroBolitasTotal :: [Color] -> Int
nroBolitasTotal cs = length cs 




vaciar :: SecuenciaDeCeldas -> SecuenciaDeCeldas
vaciar (Sec izq actual der) = 
	Sec (vaciarCeldas izq)
		(vaciarCelda actual) 
		(vaciarCeldas der)

vaciarCeldas :: [Celda] -> [Celda]
vaciarCeldas [] = []
vaciarCeldas (c:cs) =
	vaciarCelda c : vaciarCeldas cs

vaciarCelda :: Celda -> Celda
vaciarCelda (ConsCelda bs) = ConsCelda []