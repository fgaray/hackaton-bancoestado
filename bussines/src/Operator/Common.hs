-- | Common functions of the operator module
{-# LANGUAGE ScopedTypeVariables #-}
module Operator.Common where


import Operator.Types
import Import


-- | Tries to find the operator at the given index
operatorStepAt :: OperatorGraph -> Int -> Maybe OperatorStep
operatorStepAt (OperatorGraph steps) n
    | n < length steps && n >= 0 = Just $ steps !! n
    | otherwise                  = Nothing

-- | Traverses all the graph doing a depth first search applying the given
-- function in each step
traverseSteps :: forall a. (a -> OperatorStep -> a) -> a -> OperatorGraph -> a
traverseSteps _ state (OperatorGraph []) = state
traverseSteps fn state g@(OperatorGraph steps@(s:_)) = go' state s
    where
        go :: a -> [OperatorStep] -> a
        go state' ss = foldl' go' state' ss

        go' :: a -> OperatorStep -> a
        go' state' step@(OperatorStep _ n _ nexts) = go (fn state' step) (mapMaybe (operatorStepAt g) nexts)
