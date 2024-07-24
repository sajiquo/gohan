module Main (main) where
import           HeapSort  (heapSort)
import           QuickSort (quickSort)


main :: IO ()
main = do
  let listToTry = [4,1,3,2,16,9,10,14,8,7]
  print listToTry
  print "quickSort"
  print (quickSort listToTry)
  print "heapSort"
  print (heapSort listToTry)



