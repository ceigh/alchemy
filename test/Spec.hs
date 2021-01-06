import Control.Exception (evaluate)
import Data.Maybe (fromJust)
import Lens.Micro ((^.))
import Test.Hspec
-- import Text.Pretty.Simple (pPrint)
import Test.QuickCheck

import Alchemy

main :: IO ()
main = hspec $ do
  g <- runIO initGame

  describe "Alchemy" $ do
    it "has elements" $ do
      null (g ^. allElements) `shouldBe` False

    it "adds elements to desk" $ do
      let g1 = addElementToDesk 0 g
          e1 = last $ g1 ^. deskElements
          e2 = head $ g1 ^. allElements
      e1 `shouldBe` e2

    it "is not record history and change desk elements if combination\
        \ is wrong" $ do
      let g1           = combineElements 0 2 g -- air and fire
          emptyHistory = null   $ g1 ^. history
          onDeskOldNum = length $ g  ^. deskElements
          onDeskNewNum = length $ g1 ^. deskElements
      onDeskNewNum `shouldBe` onDeskOldNum
      emptyHistory `shouldBe` True

    it "is record history, remove old pair and place new element on desk\
        \ if combination is success" $ do
      let g1           = combineElements 0 1 g -- air and earth
          emptyHistory = null   $ g1 ^. history
          onDeskOldNum = length $ g  ^. deskElements
          onDeskNewNum = length $ g1 ^. deskElements
      onDeskNewNum `shouldBe` onDeskOldNum - 1
      emptyHistory `shouldBe` False

    it "throws if combining with itself" $ do
      -- air and air
      evaluate (combineElements 0 0 g) `shouldThrow` anyException

    it "select element" $ do
      let g1       = selectElement 0 g
          selected = fromJust $ g1 ^. selectedElement
      selected `shouldBe` head (g ^. deskElements)
