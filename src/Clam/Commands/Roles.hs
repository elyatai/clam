module Clam.Commands.Roles (commands) where

import Clam.Prelude
import Clam.Rdb
import Clam.Types as Clam

import Calamity
import Calamity.Commands hiding (commands)
import Database.Persist

commands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
commands = void do
  command @'[Text] "add-group" \ctx grp → do
    whenM (isLeft <$> rdbPutUq (Group grp)) $
      fail "That group already exists!"
    void $ tell @Text ctx "Group created"

  command @'[Text] "del-group" \ctx grp → do
    let uq = UqGroupName grp
    k ← rdbGetUq uq
      >>= maybe (fail "Group doesn't exist") (pure . entityKey)
    whenM (rdbHas [RoleGroup ==. k]) $
      fail "Can't delete group, roles under it exist"
    rdbDelUq uq
    void $ tell @Text ctx "Group deleted"
