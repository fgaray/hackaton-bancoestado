{-# LANGUAGE TemplateHaskell #-}
-- | Module for the Graph datatype that is going to repressent the processing
-- graph and the common internal data format to interconnect every operator
module Operator.Types where


import Data.Text (Text)
import Data.Map (Map)
import Data.Aeson.TH




-- | A common format to unify all the operators
data CommonFormat = CommonFormat !Meta ![Row]

-- | Meta internal information
data Meta = Meta

-- | A collection of named columns
data Row = Row !(Map Text Entity)

-- | An entity can be different types that can be easity expressed inside
-- haskell and one special case that cannot. This special datatype is store as a
-- text plus a collection of functions that can be applied to this datatype to
-- manipulate it
data Entity =
      EntityText !Text
    | EntityInt !Int
    | EntitySpecial !Text !EntityFunctions


-- | A collection of functions to manipulate an special entity
data EntityFunctions = EntityFunctions
    { entityValidation :: Text -> Bool
    }





data OperatorState = OperatorState
    { runningState :: !Bool       -- Is this state running?
    , blockedState :: !(Maybe Text) -- Is this state being blocked? If so then here is the reason
    } deriving (Show, Eq)

data OperatorGraph = OperatorGraph ![OperatorStep]
    deriving (Show)

-- | An step of the operator grap.
--  1) Current state of this operator
--  2) The number of the step
--  3) The operator to execute
--  4) The step outputs
data OperatorStep = OperatorStep !OperatorState !Int !Operator ![Int]
    deriving (Show, Eq)


-- | All the operators as a datatype. Inputs, Process and 
data Operator =
      Start
    | Other
    | End
    deriving (Show, Eq, Ord)


-- | Init functions

emptyCommonFormat :: CommonFormat
emptyCommonFormat = CommonFormat Meta []

defaultState :: OperatorState
defaultState = OperatorState False Nothing


-- | Json instances

$(deriveJSON defaultOptions ''Operator)
$(deriveJSON defaultOptions ''OperatorState)
