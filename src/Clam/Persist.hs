{-# LANGUAGE TemplateHaskell, QuasiQuotes, UndecidableInstances #-}
module Clam.Persist where

import Clam.Prelude

import qualified Calamity.Types as C
import Data.Time.Clock
import Database.Persist.TH

$(let settings = sqlSettings { mpsGenerateLenses = True }
  in share [mkPersist settings, mkMigrate "migrateAll"] [persistLowerCase|
User
  Id (C.Snowflake C.User)
  level Word
  xp Word

Group
  name Text

Role
  Id (C.Snowflake C.Role)
  name Text
  emoji Text
  group GroupId
  UniqueRole group emoji

Command
  name Text
  reply Text
  UniqueCommand name

Vent
  Id (C.Snowflake C.GuildChannel)
  lastMsg UTCTime Maybe
|])

deriving stock instance Generic User
deriving stock instance Generic Group
deriving stock instance Generic Role
deriving stock instance Generic Command
