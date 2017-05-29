import           Protolude
import           Test.Hspec

import           Khanren.Core
import           Khanren.State
import           Khanren.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Khanren" $ do
  describe "Khanren Core" $ do
    describe "===" $ do
      it "yields nothing when inconsistent" $ do
        (Num 3 === Num 5) emptyState `shouldBe` []

      it "yields the original state when already consistent" $ do
        (Num 3 === Num 3) emptyState `shouldBe` [emptyState]

      it "yields an extended state when a new unification is found" $ do
        (Var 17 === Num 29) emptyState `shouldBe`
          [ State [(17, Num 29)] 0 ]

    describe "callFresh" $ do
      it "generates a new var" $ do
        callFresh (\_ -> \st -> [st]) emptyState
          `shouldBe`
            [ State [] 1 ]

      it "passes the new var to the func" $ do
        callFresh (\q -> q === Num 5) emptyState
          `shouldBe`
            [ State [(0, Num 5)] 1 ]

    describe "disj" $ do
      it "generates states for both branches" $ do
        callFresh (\q -> disj (q === Num 5) (q === Num 19)) emptyState
          `shouldBe`
            [ State [(0, Num 5)] 1
            , State [(0, Num 19)] 1
            ]

    describe "conj" $ do
      it "filters states through both brances" $ do
        callFresh (\q -> conj (q === Num 5) (q === Num 19)) emptyState
          `shouldBe` []
        callFresh (\q -> conj (q === Num 5) (q === Num 5)) emptyState
          `shouldBe`
            [ State [(0, Num 5)] 1 ]

    describe "with an infinite stream" $ do
      it "generates a lazy stream" $ do
        let fives x = disj (x === Num 5) (fives x)
         in take 3 (callFresh fives emptyState)
              `shouldBe`
                [ State [(0, Num 5)] 1
                , State [(0, Num 5)] 1
                , State [(0, Num 5)] 1
                ]

      it "interleaves streams" $ do
        let fives x = disj (x === Num 5) (fives x)
            sixes x = disj (x === Num 6) (sixes x)
            fivesAndSixes x = disj (fives x) (sixes x)
         in take 4 (callFresh fivesAndSixes emptyState)
              `shouldBe`
                [ State [(0, Num 5)] 1
                , State [(0, Num 6)] 1
                , State [(0, Num 5)] 1
                , State [(0, Num 6)] 1
                ]

