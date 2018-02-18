module OperatorSpec where

import Test.Hspec
import Operator.Common
import Operator.Types
import Operator.Validate


spec :: Spec
spec = do
    describe "Common" $ return ()




{-exampleGraph :: OperatorGraph-}
{-exampleGraph = OperatorGraph steps-}
    {-where-}
        {-steps :: [OperatorStep]-}
        {-steps = [ OperatorStep 0 Start [1]-}
                {-, OperatorStep 1 Other [2]-}
                {-, OperatorStep 2 End []-}
                {-]-}


{-exampleGraphBad :: OperatorGraph-}
{-exampleGraphBad = OperatorGraph steps-}
    {-where-}
        {-steps :: [OperatorStep]-}
        {-steps = [ OperatorStep 0 Start [1]-}
                {-, OperatorStep 1 Other []-}
                {-, OperatorStep 2 End []-}
                {-]-}

{-exampleGraphBad2 :: OperatorGraph-}
{-exampleGraphBad2 = OperatorGraph steps-}
    {-where-}
        {-steps :: [OperatorStep]-}
        {-steps = [ OperatorStep 0 Start [1]-}
                {-, OperatorStep 1 Other [2]-}
                {-]-}


 {-Tests-}

{-common :: Spec-}
{-common = do-}
    {-describe "operatorStepAt" $ do-}
        {-it "Given a graph, it should return the step in the index" $ do-}
            {-(operatorStepAt exampleGraph 0) `shouldBe` (Just $ OperatorStep 0 Start [1])-}


{-validate :: Spec-}
{-validate = do-}
    {-describe "allReachable" $ do-}
        {-it "Given a graph, it should return if the graph is valid" $ do-}
            {-(allReachable (OperatorGraph [])) `shouldBe` True-}
            {-(allReachable exampleGraph) `shouldBe` True-}
            {-(allReachable exampleGraphBad) `shouldBe` False-}

    {-describe "isStartAndEnd" $ do-}
        {-it "A graph should have a start and an end" $ do-}
            {-(isStartAndEnd exampleGraph) `shouldBe` True-}
            {-(isStartAndEnd exampleGraphBad) `shouldBe` False -- Can't reach-}
            {-(isStartAndEnd exampleGraphBad2) `shouldBe` False-}
