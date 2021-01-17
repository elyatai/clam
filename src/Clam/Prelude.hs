module Clam.Prelude (module M) where

import Relude as M hiding
  ( uncons
  , Reader, runReader, ask, asks, local
  , State, runState, get, put, gets, modify
  )
import Control.Arrow as M hiding (first, second)
import Control.Lens as M hiding ((??), rewrite, transform, Context)
import DiPolysemy as M -- note: `error` conflicts with relude so omitted
  (debug, info, notice, warning, alert, critical, emergency)
import Polysemy as M
import Polysemy.Reader as M

import Clam.Orphans ()
