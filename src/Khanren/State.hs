module Khanren.State where

import           LocalPrelude

import           Data.List     (lookup)

import           Khanren.Types

allocVar :: State -> (Term, State)
allocVar (State s c) =
  (Var c, State s (succ c))

emptyState :: State
emptyState = State [] 0

walk :: Term -> Subst -> Term
walk (Var id) s =
  case lookup id s of
    Nothing -> Var id
    Just v  -> walk v s
walk (Num n) _ = Num n
