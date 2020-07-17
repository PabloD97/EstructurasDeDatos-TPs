
--Recursion 
-- 2)
-- Dada una lista de elementos devuelve true
-- si esta vacia y false en el caso contrario
isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty xs = False

-- 3)
-- dado una lista de elementos
-- devuelve el primero
head' :: [a] -> a
head' (x : xs) = x

-- 4)
-- dada una lista de elementos devuelve
-- la misma lista, sin el primer elemento
tail' :: [a] -> [a]
tail' (x:xs) = xs

--Recursion sobre listas
-- 1) 
-- dada una lista de enteros devuelve
-- la suma de todos sus elementos
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x : xs) = x + sumatoria xs

-- 2)
-- dada una lista de elementos de algun
-- tipo devuelve el largo de esa lista
longitud :: [a] -> Int
longitud [] = 0
longitud (x : xs) = 1 + longitud xs

-- 3)
-- dada una lista de enteros, devuelve la 
-- lista de los sucesores de cada entero
sucesor :: Int -> Int
sucesor nro = nro + 1

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x : xs) = sucesor x : sucesores xs

-- 4)
--Dada una lista de pares de enteros, devuelve
--una lista en la que cada elemento es la
--suma de los elementos de cada par
sumarPar :: (Int,Int) -> Int
sumarPar (nro1,nro2) = nro1 + nro2

sumaDePares :: [(Int,Int)] -> [Int]
sumaDePares [] = []
sumaDePares (x:xs)= --((x,y): xs) esta mal el pm anidado 
	sumarPar (x) : sumaDePares xs


-- 5)
-- dada una losta de booleanos devuelve
-- true si todos sus elementos 
-- son true
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x : xs) = x && conjuncion xs 


-- 6)
-- dada una lista de booleanos
-- devuelve true si alguno de sus
-- elementos son true
disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x : xs) = x || disyuncion xs 

-- 7) 
-- dados un elemento e y una lista xs
-- devuelve true si existe
-- un elemento en xs que sea igual a e
pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) = 
	(x == y) || pertenece x ys


-- 8)
-- dados un elemento e y una lista xs
-- cuenta la cantidad de apariciones de e
-- en xs
apariciones :: Eq a => a -> [a] -> Int
apariciones x [] = 0
apariciones x (y:ys) = 
	if (x == y)  
		then 1 + apariciones x ys
		else apariciones x ys


-- 9)
-- dados un numero n y una lista
-- xs, devuelve todos los elementos
-- de xs que son menores a n
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA x [] = []
losMenoresA x (y:ys) = 
	if (x > y)  
		then y : losMenoresA x ys
		else losMenoresA x ys


-- 10)
-- dados un elemento y una lista
-- filtra(elimina) todas las ocurrencias
-- de ese elemento en la lista
losDistintosA :: Eq a => a -> [a] -> [a]
losDistintosA x [] = []
losDistintosA x (y:ys) = 
	if (x /= y)  
		then y : losDistintosA x ys
		else losDistintosA x ys


-- 11)
-- Dada una lista de listas, devuelve la 
-- lista de sus longitudes. Aplique esta 
-- función a la lista de strings 
-- ["Estructuras", "de", "datos"] 
-- y observe el resultado.

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x : xs) = 
	longitud x : longitudes xs

-- 12) 
-- Dados un número n y una lista de listas, 
-- devuelve la lista de aquellas listas que 
-- tienen más de n elementos.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xss) =  
	if ( n < longitud x )
		then x : lasDeLongitudMayorA n xss
		else lasDeLongitudMayorA n  xss

-- 13)
-- Dado un elemento e y una lista xs,
-- ubica a e entre medio de todos los elementos
-- de xs.
intercalar :: a -> [a] -> [a]
-- caso base
intercalar e [] = []
-- caso recursivo
intercalar x [y] = [y] -- esto quiere decir que la lista tiene un solo elemento
intercalar e (x:xs) = x : e : intercalar e xs 

-- 14)
-- Dados una lista y un elemento, 
-- devuelve una lista con ese elemento
-- agregado al final de la lista.
snoc :: [a] -> a -> [a]
snoc [] e =  [e]
snoc (x:xs) e = 
	x : (snoc xs e)


-- 15)
-- Dadas dos listas devuelve la lista 
-- con todos los elementos de la primera lista
-- y todos los elementos de la segunda
-- a continuación.
append :: [a] -> [a] -> [a]
append [] ys= ys
append (x:xs) ys= x : append xs ys
append [] (y:ys) = y : append [] ys 
-- este es el template
-- miFuncion (x : xs) z = 
-- f x ... miFuncion xs z


-- 16) 
-- como decimos que es una lista de lista
-- (xs : xss)
aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (xs : xss) =
	append xs (aplanar xss) -- funciona


-- 17)
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) =
	reversa xs ++ [x]

-- 18)
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] [] = []
zipMaximos xs [] = xs
zipMaximos (x:xs) (y:ys) =
	--f x y ... zipMaximos xs ys
	max x y :zipMaximos xs ys

-- 19)
zipSort :: [Int] -> [Int] -> [(Int,Int)]
zipSort [] [] = []
zipSort (x:xs) (y:ys) = 
	(min x y, max x y) : (zipSort xs ys)






-- 20)
promedio  :: [Int] -> Int
promedio xs = div (sumatoria xs) (longitud xs)

-- let permite declarar una expresion 
-- let filtrado = filtrarMultiplosDe z xs in
-- ahora puedo usar filtrafo como si fuera la 
-- funcion de arriba


-- 21)
-- dada una lista devuelve el minimo
minimun :: Ord a => [a] -> a
minimun  [x] = x
minimun (x:xs) = 
	min x (minimun xs)


-- Recursion sobre numeros

-- 1)
-- Dado un número n se devuelve la 
-- multiplicación de este número y 
-- todos sus anteriores hasta llegar a 0. 
-- Si n es 0 devuelve 1. La función es parcial 
-- si n es negativo.
factorial :: Int -> [Int]
factorial 0 = [] 
factorial n = n * n :factorial (n-1) 

-- 2)
-- Dado un número n devuelve una lista cuyos
-- elementos sean los números comprendidos
-- entre n y 1 (incluidos). 
-- Si el número es inferior a 1, 
-- devuelve la lista vacía.
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

-- 3)
contarHasta :: Int -> [Int]
contarHasta 0 = []
contarHasta n = contarHasta (n-1) ++ [n]

-- 4)
replicarN :: Int -> a -> [a]
replicarN n e = 
	if n == 0
		then []
		else e : replicarN (n - 1) e

-- 5)
desdeHasta :: Int -> Int -> [Int]
desdeHasta n1 n2 = 
	if n1 <= n2
		then n1 : desdeHasta (n1 + 1) n2
		else []

-- 6)
takeN :: Int -> [a] -> [a]
takeN 0 (x:xs) = [] 
takeN n [] = []
takeN n (x:xs) = 
	if n > longitud (x:xs)
		then []
		else x : takeN (n-1) xs

-- 7)
dropN :: Int -> [a] -> [a]
dropN 0 [] = []
dropN 0 (x:xs) = (x:xs)
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs


-- 8)
splitN :: Int -> [a] -> ([a] , [a])
splitN 0 (x:xs) = ([] , dropN 0 (x:xs)) 
splitN n [] 	= ([],[])
splitN n [x]	= (takeN n [x] , dropN n [x])
splitN n (x:xs) = (takeN n (x:xs) , dropN n (x:xs))


-- Anexo con ejercicios adicionales
-- 1)
-- dada una lista devuelve el maximo
maximun :: Ord a => [a] -> a
maximun [x] = x
maximun (x:xs) =
	max x (maximun xs)

-- 2)
-- devuelve el minimo y la lista sin el
sinElemento :: Eq a => a -> [a] -> [a]
sinElemento e [] = []
sinElemento e (x:xs) =
	if e == x 
		then sinElemento e xs
		else x : sinElemento e xs

splitMin :: Ord a => [a] -> (a, [a])
splitMin [x] 	= (x, [x])
splitMin xs = 
	( minimun xs ,  sinElemento (minimun xs) xs )


-- 3)
-- dada cualquier lista la devuelve 
-- ordenada de menor a mayor
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar xs =
	minimun xs : ordenar (sinElemento (minimun xs) xs)


-- 4)
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] xs = []
interseccion xs [] = []
interseccion (x:xs) ys =
	if pertenece x ys
		then x : interseccion xs ys
		else interseccion xs ys

-- 5)
diferencia :: Eq a => [a] -> [a] -> [a]
diferencia xs [] = xs
diferencia [] ys = []
diferencia (x:xs) (y:ys) =
	if pertenece x ys
		then diferencia xs ys
		else x : diferencia xs ys

-- 6)
agregarSegunSigno :: Int -> ([Int],[Int]) -> ([Int],[Int])
agregarSegunSigno n (xs,ys) =
	if n >= 0
		then (n:xs , ys)
		else (xs , n:ys)

particionPorSigno :: [Int] -> ([Int] , [Int])
particionPorSigno [] = ([],[])
particionPorSigno (x:xs) =
	agregarSegunSigno x (particionPorSigno xs)



-- 8) 
subtails :: [a] -> [[a]]
subtails [] = []
subtails [x] = [[x]]
subtails xs = xs : subtails (tail' xs)

-- 9)
agrupar :: Eq a => [a] -> [[a]]
agrupar [] = []
agrupar (x:xs)= agregarAlPrincipio x (agrupar xs)


agregarAlPrincipio :: Eq a => a ->[[a]] -> [[a]]
agregarAlPrincipio n [] = [[n]]
agregarAlPrincipio n (x : xs) = 
	if n == head x
		then (n:x) :  xs
		else [n] : (x:xs)

 -- funciones recursivas
 -- debe iterarse sobre alguna estructura
 -- puede ser una lista, o incluso un numero
 -- se debe contemplar el caso base
 -- diferentes x [] =

-- se debe contemplar el caso recursivo
-- se debe evaluar el llamado recursivo
--  difrente ys
-- el llamado recursivo tiene que consumir la
-- estructura si es un numero (i - l) por 
-- ejemplo
-- si es una lista de tipo (x:xs) se debe 
-- utilizar xs por ultimo seguramente
-- debamos el valor actual de la recursion
-- y construir la estructura que querramos
-- retornar uniendo
-- el resultado de modificar el valor actual
-- y el llamado recursivo
-- miFuncion x (y:ys)=
	--f y : miFuncion x xy

