module Khanren.Unify where

import           LocalPrelude

import           Khanren.State
import           Khanren.Types

unify :: Term -> Term -> Subst -> UResult
unify u v s =
  case (walk u s, walk v s) of
    (Var id1, Var id2) ->
      if id1 == id2
        then Nop
        else Extend id1 (Var id2)
    (Var id, Num n) ->
      Extend id (Num n)
    (Num n, Var id) ->
      Extend id (Num n)
    (Num n, Num m) ->
      if n == m
        then Nop
        else Reject
