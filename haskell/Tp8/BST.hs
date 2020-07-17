data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
   deriving Show

-- Binary Search Tree (arboles de busqueda binaria)
-- Invariantes:
-- Dado un arbol NodeT x ti td
-- 1) todos los elementos de ti
--    son menores a x
-- 2) todos los elementos de td
--    son mayores a x
-- 3) ti y td son BST
-- 4) Opcional: no hay elementos repetidos

arbolInt:: Tree Int
arbolInt = NodeT 8 (NodeT 4 EmptyT EmptyT) (NodeT 13 EmptyT EmptyT) 

-- Prec.: el arbol es BST
-- Eficiencia: O(log n)
perteneceBST :: Ord a => a -> Tree a -> Bool
perteneceBST e EmptyT = False
perteneceBST e (NodeT x ti td) =
	if e == x
	   then True
	   else if e < x
	   	       then perteneceBST e ti
	   	       else perteneceBST e td

-- Prec.: el arbol es BST
-- Eficiencia: O(log n)
lookupBST :: Ord k => k -> Tree (k, v) -> Maybe v
lookupBST k EmptyT = Nothing
lookupBST k (NodeT p ti td) =
	if k == fst p
	   then Just (snd p)
	   else if k < fst p
	   	       then lookupBST k ti
	   	       else lookupBST k td

-- Prec.: el arbol es BST
-- Proposito: dado un arbol BST
-- devuelve un arbol BST
-- Eficiencia: O(log n)
insertT :: Ord a => a -> Tree a -> Tree a
insertT e EmptyT = NodeT e EmptyT EmptyT
insertT e (NodeT x ti td) = 
	if e == x
	   then NodeT x ti td
	   else if e < x
	   	       then NodeT x (insertT e ti) td
	   	       else NodeT x ti (insertT e td)

root :: Tree a -> a
root (Nodet a ti td) = a


-- Dado un BST borra un elemento
-- en el arbol
deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST a (NodeT x ti td) =
	