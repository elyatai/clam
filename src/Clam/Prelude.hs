module Clam.Prelude (module M) where

import Relude as M hiding
  ( uncons, error
  , Reader, runReader, ask, asks, local
  , State, runState, get, put, gets, modify
  )
import Control.Arrow as M hiding (first, second)
import Control.Lens as M hiding ((??), rewrite, transform, Context)
import DiPolysemy as M
  (debug, info, notice, warning, error, alert, critical, emergency)
import Polysemy as M
import Polysemy.Reader as M
import Polysemy.Fail as M
import TextShow as M (showt, showtl)

import Clam.Orphans ()
