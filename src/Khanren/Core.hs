module Khanren.Core
  ( (===)
  , callFresh
  , disj
  , conj
  ) where

import           LocalPrelude

import           Khanren.State
import           Khanren.Types
import           Khanren.Unify


infix 4 ===

(===) :: Term -> Term -> Goal
u === v = \st ->
  case unify u v (_subst st) of
    Reject         -> []
    Nop            -> [st]
    Extend id term -> [st { _subst = s' }] where s' = (id, term) : _subst st

callFresh :: (Term -> Goal) -> Goal
callFresh toGoal st =
  let (t, st') = allocVar st
   in toGoal t st'

disj :: Goal -> Goal -> Goal
disj a b st =
  interleave (a st) (b st)

interleave :: [a] -> [a] -> [a]
interleave [] bs     = bs
interleave (a:as) bs = a : interleave bs as

conj :: Goal -> Goal -> Goal
conj a b st =
  let sts = a st
   in concatMap b sts
