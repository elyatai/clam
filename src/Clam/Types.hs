module Clam.Types
  ( Clam.Types.BotC
  , Config
  , module Clam.Rdb
  , module Clam.Persist
  ) where

import Clam.Prelude
import Clam.Config
import Clam.Rdb (RdbC, Rdb(..))
import Clam.Persist hiding (migrateAll)

import Calamity
import Calamity.Commands
import Database.Persist.Sql (SqlBackend)

type BotC r =
  ( Calamity.BotC r
  , Members '[ParsePrefix, Reader Config, Rdb SqlBackend] r
  )
