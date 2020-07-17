import BinaryHeap

-- Interfaz a usar:
-- emptyH
-- isEmptyH
-- insertH
-- findMin
-- deleteMin
-- splitMin


heapSort :: Ord a => [a] -> [a]
heapSort xs = binaryHeapToList (listToBinaryHeap xs)

binaryHeapToList :: Ord a => Heap a -> [a]
binaryHeapToList heap =
	if isEmptyH heap
		then []
		else findMin heap : binaryHeapToList (deleteMin heap)

listToBinaryHeap :: Ord a => [a] -> Heap a
listToBinaryHeap []     = emptyH
listToBinaryHeap (x:xs) = 
	insertH x (listToBinaryHeap xs) 
