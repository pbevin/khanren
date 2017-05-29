module Khanren.Types where

import           LocalPrelude


type Subst = [(Int, Term)]

data State = State
  { _subst  :: Subst
  , _nextID :: Int
  } deriving (Show, Eq)

type Goal = State -> [State]

data Term
  = Num Int
  | Var Int
  deriving (Show, Eq)

-- Unification result
data UResult
  = Reject
  | Nop
  | Extend Int Term
  deriving (Show, Eq)
