import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Lens.Micro ((^.))
import Alchemy

main :: IO ()
main = hspec $ do
  g <- runIO initGame

  describe "Alchemy" $ do
    it "has elements" $ do
      null (g ^. allElements) `shouldBe` False

    it "adds elements to desk" $ do
      let g1 = addElementToDesk 0 g
          e1 = head $ g1 ^. deskElements
          e2 = head $ g1 ^. allElements
      e1 `shouldBe` e2

    {-
    it "throws an exception if used with an empty list" $ do
      evaluate (head []) `shouldThrow` anyException
    -}

{-
  let g1 = addElementToDesk 0 g    -- test adding
      g2 = addElementToDesk 1 g1
      g3 = combineElements  0 1 g2 -- test success combine
      g4 = combineElements  0 1 g3 -- test fail combine
      g5 = selectElement    2 g4   -- test selection
      g6 = combineElements  0 1 g5 -- test fail combine
      g7 = combineElements  0 0 g6 -- test error combine
-}
