import Stack


data Tree a =
	  EmptyT
	| NodeT a (Tree a) (Tree a) deriving Show


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

-- dada una lista devuelve
-- una pila sin alterar el 
-- orden de los elementos
apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) =
	push x (apilar xs) 

-- dada una pila devuelve una 
-- lista sin alterar el orden
-- de los elementos
desapilar :: Stack a -> [a]
desapilar pila =
	if isEmptyS pila
		then []
		else top pila : desapilar (pop pila)

-- (push 4 $ push 5 $ push 90 $ push 32 $ push 54 emptyS)

treeToStack :: Tree a -> Stack a
treeToStack arbol =
	apilar (listInOrder arbol)


listInOrder :: Tree a -> [a]
listInOrder EmptyT = []
listInOrder (NodeT x ti td) = 
	(listInOrder ti) ++ [x] ++ (listInOrder td)  


----------------------------------------------------------------------

