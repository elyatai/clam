module Clam.Prelude (module M, map) where

import Relude as M hiding
  ( uncons, error, map
  , Reader, runReader, ask, asks, local
  , State, runState, evalState, execState, get, put, gets, modify, modify'
  )
import Control.Arrow as M hiding (first, second)
import Control.Lens as M hiding ((??), rewrite, transform, Context)
import DiPolysemy as M
  (debug, info, notice, warning, error, alert, critical, emergency)
import Polysemy as M
import Polysemy.Reader as M
import Polysemy.State as M
import Polysemy.Fail as M
import TextShow as M (showt, showtl)

import Clam.Orphans ()

map ∷ Functor f ⇒ (a → b) → f a → f b
map = fmap
{-# INLINE map #-}
