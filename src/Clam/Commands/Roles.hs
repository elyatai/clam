module Clam.Commands.Roles (commands) where

import Clam.Prelude
import Clam.Sql
import Clam.Types as Clam

import Calamity as D hiding (emoji, parse)
import Calamity.Commands hiding (commands)
import qualified Data.Text.Lazy as L
import Database.Persist

commands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
commands = void do
  command @'[Text] "add-group" \ctx grp → do
    whenM (isLeft <$> sqlPutUq (Group grp)) $
      fail "That group already exists!"
    void $ tell @Text ctx "Group created"

  command @'[Text] "del-group" \ctx grp → do
    k ← groupFromName grp
    whenM (sqlHas [RoleGroup ==. k]) $
      fail "Can't delete group, roles under it exist"
    sqlDelUq $ UqGroupName grp
    void $ tell @Text ctx "Group deleted"

  command @'[Text, Snowflake D.Role, RawEmoji] "track-role" \ctx grp role emoji → do
    let emoji' = showt emoji
    gk ← groupFromName grp

    whenJustM (sqlGetUq $ UqRole gk emoji') \(Entity (RoleKey rid) _) → do
      g ← whenNothing (ctx ^. #guild) $ fail "You're not in a server!"
      rest ← invoke (GetGuildRoles g)
        >>= \case
          Left e → do
            error $ show @Text e
            fail "Something went seriously wrong"
          Right rs → pure rs
        <&> find ((== rid) . getID)
        <&> maybe ", but could not be loaded" \r → ": " <> r ^. #name
      fail . L.unpack $
        "A role in group " <> codeline (toLazy grp)
        <> " with emoji " <> showtl emoji
        <> " already exists" <> rest

    sqlPut' (RoleKey role) $ Clam.Role emoji' gk
    void $ tell @Text ctx "Role tracked"

  command @'[RoleRef] "untrack-role" \ctx rref → do
    mrole ← case rref of
      ById rid → ($> RoleKey rid) <$> sqlGet (RoleKey rid)
      ByGroupEmoji g e → do
        gk ← groupFromName g
        fmap entityKey <$> sqlGetUq (UqRole gk $ showt e)
    rk ← whenNothing mrole $ fail "Role not found, nothing done"
    sqlDel rk
    void $ tell @Text ctx "Role untracked"

data RoleRef = ById (Snowflake D.Role) | ByGroupEmoji Text RawEmoji

instance Parser RoleRef r where
  parse = parse @(Either (Snowflake D.Role) (Text, RawEmoji))
    <&> either ById (uncurry ByGroupEmoji)

groupFromName ∷ Members [Fail, Sql SqlBackend] r ⇒ Text → Sem r (Key Group)
groupFromName grp = sqlGetUq (UqGroupName grp)
  >>= maybe (fail "Group doesn't exist") (pure . entityKey)
