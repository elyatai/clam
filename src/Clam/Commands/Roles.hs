module Clam.Commands.Roles (commands) where

import Clam.Prelude
import Clam.Sql
import Clam.Types as Clam
import Clam.Utils.Calamity

import Calamity as D hiding (emoji, parse, Member)
import Calamity.Commands hiding (commands)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Database.Esqueleto.Experimental (From(..))
import qualified Database.Esqueleto.Experimental as E
import Database.Persist as P

commands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
commands = void do
  addGroupCmd
  delGroupCmd
  listGroupsCmd

  trackRoleCmd
  untrackRoleCmd

addGroupCmd ∷ Cmd r
addGroupCmd = command_ @'[Text] "add-group" $ \ctx grp → do
  whenM (isLeft <$> sqlPutUq (Group grp)) $
    fail "That group already exists!"
  void $ tell @Text ctx "Group created"

delGroupCmd ∷ Cmd r
delGroupCmd = command_ @'[Text] "del-group" \ctx grp → do
  k ← groupFromName grp
  whenM (sqlHas [RoleGroup ==. k]) $
    fail "Can't delete group, roles under it exist"
  sqlDelUq $ UqGroupName grp
  void $ tell @Text ctx "Group deleted"

listGroupsCmd ∷ Cmd r
listGroupsCmd = command_ @'[] "list-groups" \ctx → do
  gs ← sqlGetQ $ E.from $ Table @Group
  rs ← sqlGetQ $ E.from $ Table @Clam.Role
  map (\(Entity k g) → (k, (g ^. groupName, []))) gs
    & M.fromList
    & flip (foldl' $ flip consRole) rs
    & T.unlines . map (uncurry fmt) . M.elems
    & void . tell ctx
  where
    consRole (Entity _ r) = ix (r ^. roleGroup) . _2 %~ (r :)
    fmt grp [] = grp <> " (0)"
    fmt grp rs = grp <> " (" <> showt (length rs) <> "): " <>
      T.intercalate ", " (rs ^.. traversed . roleEmoji)

trackRoleCmd ∷ Cmd r
trackRoleCmd = command_ @'[Text, Snowflake D.Role, RawEmoji] "track-role"
  \ctx grp role emoji → do
    let emoji' = showt emoji
    gk ← groupFromName grp

    whenJustM (sqlGetUq $ UqRole gk emoji') \(Entity (RoleKey rid) _) → do
      gid ← ctx ^? #guild . _Just . #id
        & maybe (asks @Config $ view #guild) pure
      rest ← upgrade (gid, rid)
        <&> maybe ", but could not be loaded" \r → ": " <> r ^. #name
      fail . L.unpack $
        "A role in group " <> codeline (toLazy grp)
        <> " with emoji " <> showtl emoji
        <> " already exists" <> rest

    sqlPut' (RoleKey role) $ Clam.Role emoji' gk
    void $ tell @Text ctx "Role tracked"

untrackRoleCmd ∷ Cmd r
untrackRoleCmd = command_ @'[RoleRef] "untrack-role" \ctx rref → do
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
