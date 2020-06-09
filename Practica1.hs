
--1
sucesor :: Integer -> Integer
sucesor nro = nro + 1

--2
sumar :: Integer -> Integer -> Integer
sumar nro1 nro2 = nro1 + nro2

--3
maximo :: Integer -> Integer -> Integer
maximo nro1 nro2 = 
	if nro1 > nro2
		then nro1
		else nro2


-- Tuplas
-- 3

--a 
primera :: (Integer,Integer) -> Integer
primera (nro1,nro2) = nro1

--b
segunda :: (Integer,Integer) -> Integer
segunda (nro1,nro2) = nro2

--c
sumarPar :: (Integer,Integer) -> Integer
sumarPar (nro1,nro2) = nro1 + nro2

--d
maxDelPar :: (Integer,Integer) -> Integer
maxDelPar (nro1,nro2) = 
	if nro1 > nro2
		then nro1
		else nro2

--Tipos enumerativos
--4

data Dir = Norte | Sur | Este | Oeste deriving Show

--a
opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Sur = Norte
opuesto Este = Oeste
opuesto Oeste = Este

--b
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte

--5
data DiaDeSemana = Domingo | Lunes | Martes | Miercoles | Jueves | Viernes | Sabado deriving Show

--a
primerDia :: DiaDeSemana 
primerDia = Domingo

--b
ultimoDia :: DiaDeSemana 
ultimoDia = Sabado

--c
nroDeDia :: DiaDeSemana -> Integer
nroDeDia Domingo 	= 1
nroDeDia Lunes 		= 2
nroDeDia Martes 	= 3
nroDeDia Miercoles  = 4
nroDeDia Jueves 	= 5
nroDeDia Viernes 	= 6
nroDeDia Sabado 	= 7

--d
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Domingo 	= False
empiezaConM Lunes 		= False
empiezaConM Martes 		= True
empiezaConM Miercoles 	= True
empiezaConM Jueves 		= False
empiezaConM Viernes 	= False
empiezaConM Sabado 		= False


--e
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Domingo 	= False
estaEnElMedio Lunes 	= True
estaEnElMedio Martes 	= True
estaEnElMedio Miercoles = True
estaEnElMedio Jueves 	= True
estaEnElMedio Viernes 	= True
estaEnElMedio Sabado 	= False


--f
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues dia1 dia2 = nroDeDia dia1 > nroDeDia dia2


--6
--a
negar :: Bool -> Bool
negar True  = False
negar False = True 

--b
and :: Bool -> Bool -> Bool
and True True  = True
and True False = False
and False True = False
and False False = False

--c
or :: Bool -> Bool -> Bool
or True False = True
or False True = True
or False False = False
or True True = True


--funciones polimorficas
--7
--a
loMismo :: a -> a
loMismo a = a

--b
siempreSiete :: a -> Integer
siempreSiete a = 7

--c
duplicar :: a -> (a,a)
duplicar a = (a,a)

--d
loPrimero :: a -> b -> a
loPrimero a b = a

--e
loSegundo :: a -> b -> b
loSegundo a b = b






