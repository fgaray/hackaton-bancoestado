-- | A collection of functions to validate if a graph is correctly build or not.
--
-- What to check:
--  * All connected: Can I reach every state?
--  * Is there a start and and end?
-- 
module Operator.Validate where

import qualified Data.Set as S

import Import
import Operator.Types
import Operator.Common


-- | Checks if we can reach every state in the graph
allReachable :: OperatorGraph -> Bool
allReachable g@(OperatorGraph steps) = check $ traverseSteps fn S.empty g
    where
        -- If in the set the number of steps is the same that all the avalaible
        -- steps, then we were able to reach every state
        check :: S.Set Int -> Bool
        check set
            | S.null set = True
            | otherwise   = length (S.toList set) == (length steps - 1) -- -1 for the START state

        fn :: S.Set Int -> OperatorStep -> S.Set Int
        fn set (OperatorStep _ _ Start _) = set -- ignore the first state
        fn set (OperatorStep _ n _ _)     = S.insert n set

-- | Check if we have an end and a start
isStartAndEnd :: OperatorGraph -> Bool
isStartAndEnd g@(OperatorGraph steps) = check $ traverseSteps fn S.empty g
    where
        -- Only two states, Start and End
        check :: S.Set Operator -> Bool
        check set = length (S.toList set) == 2

        fn :: S.Set Operator -> OperatorStep -> S.Set Operator
        fn set (OperatorStep _ _ name _) =
            case name of
                Start -> S.insert Start set
                End   -> S.insert End set
                _     -> set
