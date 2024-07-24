
module HeapSort
    ( heapSort
    ) where

exchange :: Int -> Int -> [a] -> [a]
exchange i j xs = exchange' i (xs!!j) (exchange' j (xs!!i) xs)

exchange' :: Int -> a -> [a] -> [a]
exchange' k x ys = take k ys ++ [x] ++ drop (k+1) ys

detectLargest :: Ord a => Int -> Int -> [a] -> Int
detectLargest idx heapSize xs = lrComp
  where left = idx * 2 + 1
        right = idx * 2 + 2
        -- なんかもっとやりようあるかも……
        lComp = if left < heapSize && xs!!idx < xs!!left then left else idx
        lrComp = if right < heapSize && xs!!lComp < xs!!right then right else lComp

maxHeapify :: Ord a => Int -> Int -> [a] -> [a]
maxHeapify idx heapSize xs =
  if largest /= idx then maxHeapify largest heapSize (exchange idx largest xs)
  else xs
  where largest = detectLargest idx heapSize xs


buildMaxHeap :: Ord a => [a] -> [a]
buildMaxHeap xs = buildMaxHeap' (length xs `div` 2) xs

buildMaxHeap' :: Ord a => Int -> [a] -> [a]
buildMaxHeap' idx xs
  | idx > 0 = buildMaxHeap' (idx - 1) (maxHeapify idx (length xs) xs)
  | idx == 0 = maxHeapify 0 (length xs) xs
  | otherwise = xs -- dead code for safety

heapSort :: (Ord a) => [a] -> [a]
heapSort xs = heapSort' (length xs - 1) (buildMaxHeap xs)

heapSort' :: (Ord a) => Int -> [a] -> [a]
heapSort' i xs
  | i > 1 = heapSort' (i-1) (maxHeapify 0 i (exchange 0 i xs))
  | i == 1  = maxHeapify 0 1 (exchange 0 1 xs)
  | otherwise = xs -- dead code for safety

