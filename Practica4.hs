
-- 1)=
data Pizza = Prepizza 
			| Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa 
				 | Queso 
				 | Jamon 
				 | Aceituna Int deriving Show


soloQueso = Capa Queso Prepizza
jamonYQueso = Capa Jamon soloQueso
completa = Capa (Aceituna 4) jamonYQueso

capas :: Pizza -> [Ingrediente]
capas Prepizza = []
capas (Capa ing cs) =
	ing : capas cs


--
esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

tieneJamon :: Pizza -> Bool
tieneJamon Prepizza = False
tieneJamon (Capa ing cs) =
	if esJamon ing
		then True
		else tieneJamon cs

--
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa ing cs) = 
	if esJamon ing 
		then sacarJamon cs
		else Capa ing (sacarJamon cs)

--
armarPizza :: [Ingrediente] -> Pizza
armarPizza []      = Prepizza
armarPizza (i:ing) =
	Capa i (armarPizza ing)

--

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza       = Prepizza
duplicarAceitunas (Capa ing cs ) =
	if esAceituna ing 
		then Capa(dobleAceituna ing)
			 (duplicarAceitunas cs)
		else Capa ing (duplicarAceitunas cs)
		

esAceituna :: Ingrediente -> Bool
esAceituna (Aceituna x) = True
esAceituna _			= False

dobleAceituna :: Ingrediente -> Ingrediente
dobleAceituna (Aceituna x) = (Aceituna (x*2))		

--
cantIngredientesPorPizza :: [Pizza] -> [(Int,Pizza)]
cantIngredientesPorPizza [] = []
cantIngredientesPorPizza (p:ps) =
	parIngredientesPorPizza p 
	: cantIngredientesPorPizza ps


parIngredientesPorPizza :: Pizza -> (Int , Pizza)
parIngredientesPorPizza p =
	( length (capas p)  , p )


-- 2)
data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin 
			  |Cofre [Objeto] Camino 
			  | Nada Camino deriving Show

recorrido1 =
	Cofre [Tesoro]$
		Nada $
			Cofre [Tesoro,Tesoro] $
				Nada $
					Cofre [Tesoro,Tesoro] $
						Fin

recorrido2 =
	Nada$
		Nada$
			Fin

hayTesoro :: Camino -> Bool
hayTesoro (Cofre objs c) =
	tieneTesoro objs || hayTesoro c

hayTesoro (Nada c) = False || hayTesoro c
hayTesoro Fin   = False


tieneTesoro :: [Objeto] -> Bool
tieneTesoro [] = False
tieneTesoro (o:objs) = 
	esTesoro o || tieneTesoro objs


esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _		= False

--
pasosHastaTesoro :: Camino -> Int
pasosHastaTesoro (Cofre o c) =
	if tieneTesoro o 
		then 0
		else 1 + pasosHastaTesoro c
pasosHastaTesoro (Nada c) =
	1 + pasosHastaTesoro c
pasosHastaTesoro Fin = 1

--

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn n camino = 
	hayTesoro (irAlCamino n camino) 


irAlCamino :: Int -> Camino -> Camino
irAlCamino n (Nada cs)       =
	if n == 0
		then (Nada Fin)
		else irAlCamino (n-1) cs

irAlCamino n (Cofre objs cs) =
	if n == 0
		then (Cofre objs Fin)
		else irAlCamino (n-1) cs	

irAlCamino n Fin = Fin

--

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n c =
	if n <= cantTesorosCamino c
		then True
		else False 

cantTesorosCamino :: Camino -> Int
cantTesorosCamino (Cofre o c)=
	tesorosPorCofre o + cantTesorosCamino c 
cantTesorosCamino (Nada c)   = 
	0 + cantTesorosCamino c
cantTesorosCamino Fin = 0

tesorosPorCofre :: [Objeto] -> Int
tesorosPorCofre [] = 0
tesorosPorCofre (o:objs) =
	if esTesoro o 
		then 1 + tesorosPorCofre objs
		else 0 + tesorosPorCofre objs

--
cantTesorosEntre :: Int -> Int -> Camino -> Int 
cantTesorosEntre n1 n2 camino =
	if n2 >= n1
		then cantTesorosCamino (irAlCamino (n2) camino ) + cantTesorosEntre n1 (n2-1) camino
		else 0


-- 3)
data ListaNoVacia a = Unit a 
					| Cons a (ListaNoVacia a) deriving Show

lista1 :: ListaNoVacia Int
lista1 = 
	Cons 9 $
		Cons 2$
			Cons 3$
				Unit 4 
-- 	
length' :: ListaNoVacia a -> Int
length' (Unit a)   = 1
length' (Cons a l) = 1 + length' l 


head' :: ListaNoVacia a -> a 
head' (Cons a l) = a
head' (Unit a )  = a

tail' :: ListaNoVacia a -> ListaNoVacia a
tail' (Unit a)   = (Unit a)
tail' (Cons a l) = l

minimo :: ListaNoVacia Int -> Int
minimo (Unit a)    = a 
minimo (Cons a l ) = min a ( minimo l )



data T a = A -- casos bases A, B a, C a a 
		 | B a 
		 | C a a 
		 | D (T a)
		 | E a (T a)

bInt :: T Int
bInt = B 123

cIntInt :: T Int
cIntInt = C 1 2

dDB :: T Int
dDB = D (D (B 123))

d1 :: T Int
d1 = D $
      D $
       D $
        B 123

eATA :: T Int
eATA = E 4 (D (C 1 3))

bList = B ["hola", "mundo"]

-- Retorna la cantidad de
-- elementos de una estructura T.
size :: T a -> Int
size (A) 	 = 1
size (B a)   = 1
size (C a b) = 1
size (D a)   = 1 + (size a)
size (E a (ts)) = 1 + (size ts)

-- Retorna la suma de todos los elementos.
sum' :: T Int -> Int
sum' (A)     	= 0
sum' (B a)   	= a
sum' (C a b) 	= a + b
sum' (D a)   	= sum' a
sum' (E a (ts)) = a + (sum' ts)


-- Indica si existe al
-- menos una aparición de D en la estructura.
hayD :: T a -> Bool
hayD (A)     	 = False
hayD (B a)   	 = False
hayD (C a b) 	 = False
hayD (D a)   	 = True  || (hayD a) 
hayD (E a (ts))  = False || (hayD ts)


-- Retorna la cantidad de E que existen en la estructura.
cantE :: T a -> Int
cantE (A) 		= 0 
cantE (B a) 	= 0
cantE (C a b ) 	= 0
cantE (D a) 	= cantE a
cantE (E a (ts))= 1 + (cantE ts)


-- Retorna los valores de cada constructor C.
--recolectarC :: T a -> [(a, a)]

toList :: T a -> [a]
toList A 		  = []
toList (B a)      = [a]
toList (C a b)    = [a,b]
toList (D a)      = toList a
toList (E a (ts)) = [a] ++ (toList ts)