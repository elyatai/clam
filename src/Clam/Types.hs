module Clam.Types
  ( Clam.Types.BotC
  , Config
  , module Clam.Sql
  , module Clam.Persist
  ) where

import Clam.Prelude
import Clam.Config
import Clam.Sql (SqlC, Sql(..), SqlBackend)
import Clam.Persist hiding (migrateAll)

import Calamity
import Calamity.Commands

type BotC r =
  ( Calamity.BotC r
  , Members '[ParsePrefix, Reader Config, Sql SqlBackend] r
  )
