-- 1. Calculo de costos
{-
eficiencia = O(1)
head’ :: [a] -> a
head’ (x:xs) = x

eficiencia = O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

eficiencia = O(n)
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

eficiencia = O(n)
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

eficiencia = O(n^2)
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs

eficiencia = O(n^2)
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs

eficiencia = O(n^2)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =
if pertenece x xs
then sinRepetidos xs
else x : sinRepetidos xs

eficiencia = O(n)
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys

eficiencia = O(n)
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

eficiencia = 0(n) 
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

eficiencia = O(n)
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

eficiencia = O(n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

eficiencia = 0(n)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

eficiencia = O(n)
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =
if n == x
then xs
else x : sacar n xs

eficiencia = O(n)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =
let m = minimo xs
in m : ordenar (sacar m xs)

-}


-- 2. Set (conjunto)
data Set a = Conjunto [a] deriving Show



set1 = Conjunto [1,2,3,4,5]


set2 = Conjunto [1,2,3,8,9,302]


-- crea un conjunto vacio
emptyS :: Set a
emptyS = Conjunto []

-- dados un elemento y un conjunto, agrega el
-- agrega el elemento al conjunto
addS :: Eq a => a -> Set a -> Set a
addS a (Conjunto [])    = Conjunto [a]
addS a (Conjunto elems) = 
	if elem a elems 
		then error "No se pueden agregar elementos repetidos"
		else Conjunto (a : elems)

-- dados un elemento y un conjunto indica si el
-- elemento pertenece al conjunto
belongs :: Eq a => a -> Set a -> Bool
belongs e (Conjunto [])    = False
belongs e (Conjunto elems) = elem e elems

-- devuelve la cantidad de elementos
-- distintos de un conjunto
sizeS :: Eq a => Set a -> Int
sizeS (Conjunto [])    = 0
sizeS (Conjunto elems) = 
	length elems

-- borra un elemento del conjunto
removeS :: Eq a => a -> Set a -> Set a
removeS a (Conjunto [])    = Conjunto []
removeS a (Conjunto elems) = 
	Conjunto (remover a elems)

remover :: Eq a => a -> [a] -> [a]
remover a []     = []
remover a (x:xs) = 
	if a == x
		then xs
		else x : remover a xs

-- dados dos conjuntos devuelve un conjunto
-- con todos los elementos de ambos conjuntos
unionS :: Eq a => Set a -> Set a -> Set a
unionS (Conjunto xs) (Conjunto ys) =
	Conjunto (unionSinRepetidos xs ys)

unionSinRepetidos :: Eq a => [a] -> [a] -> [a]
unionSinRepetidos [] ys = ys
unionSinRepetidos (x:xs) ys =
	if elem x ys
		then unionSinRepetidos xs ys
		else x : unionSinRepetidos xs ys


-- dados dos conjuntos devuelve
-- un conjunto con todos los elementos
-- en comun entre ambos
intersectionS :: Eq a => Set a -> Set a -> Set a
intersectionS (Conjunto xs) (Conjunto ys) =
	Conjunto (intersectionSinRepetidos xs ys)

intersectionSinRepetidos :: Eq a => [a] -> [a] -> [a]
intersectionSinRepetidos [] ys = []
intersectionSinRepetidos (x:xs) ys =
	if elem x ys
		then x:intersectionSinRepetidos xs ys
		else intersectionSinRepetidos xs ys

-- dado un conjunto devuelve una lista con todos
-- los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]
setToList (Conjunto a) = a
