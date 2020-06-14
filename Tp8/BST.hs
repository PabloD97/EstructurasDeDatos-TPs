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

-- insertT 20 (insertT 10 (insertT 5 (insertT 15 EmptyT)))
-- NodeT 15 
--     (NodeT 5
--     	EmptyT 
--     	(NodeT 10 
--     		EmptyT
--     		EmptyT))
--     (NodeT 20 
--     	EmptyT 
--     	EmptyT)

-- NodeT 4 
--    (NodeT 3 
--    	  (NodeT 2 
--    	  	(NodeT 1 
--    	  		EmptyT
--    	  		EmptyT) 
--    	  	EmptyT) 
--    	  EmptyT) 
--    EmptyT