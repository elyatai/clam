{-# LANGUAGE TemplateHaskell, QuasiQuotes, UndecidableInstances #-}
module Clam.Persist where

import Clam.Prelude

import qualified Calamity.Types as C
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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
|]
