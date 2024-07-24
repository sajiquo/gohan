module QuickSortSpec where

import           QuickSort  (quickSort)
import           Test.Hspec

spec :: Spec
spec = do
  describe "sort" $ do
    it "sort test" $ do
      let listToTry = [4,1,3,2,16,9,10,14,8,7]
      let expected = [1,2,3,4,7,8,9,10,14,16]
      quickSort listToTry `shouldBe` expected
