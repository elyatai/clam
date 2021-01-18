module Clam.Commands.Roles (modCommands, userCommands) where

import Clam.Prelude
import Clam.Sql
import Clam.Types as Clam
import Clam.Utils.Calamity

import Calamity as D hiding (emoji, parse, roleName, Member)
import Calamity.Commands hiding (commands)

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Database.Esqueleto.Experimental (From(..))
import qualified Database.Esqueleto.Experimental as E
import Database.Persist as P
import qualified Data.HashSet as HS

modCommands ∷ Cmd r
modCommands = do
  addGroupCmd
  delGroupCmd
  listGroupsCmd

  trackRoleCmd
  untrackRoleCmd
  listRolesCmd
  makeRoleCmd

userCommands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
userCommands = do
  addRolesCmd

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

data RoleRef = ById (Snowflake D.Role) | ByGroupEmoji Text RawEmoji

instance Parser RoleRef r where
  parse = parse @(Either (Snowflake D.Role) (Text, RawEmoji))
    <&> either ById (uncurry ByGroupEmoji)

untrackRoleCmd ∷ Cmd r
untrackRoleCmd = command_ @'[RoleRef] "untrack-role" \ctx rref → do
  mrole ← case rref of
    ById rid → ($> RoleKey rid) <$> sqlGet (RoleKey rid)
    ByGroupEmoji g e → do
      gk ← groupFromName g
      map entityKey <$> sqlGetUq (UqRole gk $ showt e)
  rk ← whenNothing mrole $ fail "Role not found, nothing done"
  sqlDel rk
  void $ tell @Text ctx "Role untracked"

listRolesCmd ∷ Cmd r
listRolesCmd = command_ @'[Text] "list-roles" \ctx grp → do
  gk ← groupFromName grp
  rs ← M.elems <$> getRolesInGroup gk
  void . tell ctx $
    "Group ``" <> grp <> "`` has " <> showt (length rs) <> " roles:\n"
    <> T.unlines (map (uncurry fmt) rs)
  where
  fmt c md = "- " <> c ^. roleEmoji <> case md of
    Nothing → " (deleted)"
    Just d  → ": " <> toStrict (d ^. #name)

makeRoleCmd ∷ Cmd r
makeRoleCmd = command_ @'[Text, Text, RawEmoji] "make-role"
  \ctx grp roleName emoji → do
    gid ← asks @Config $ view #guild
    gk ← groupFromName grp
    whenJustM (sqlGetUq $ UqRole gk $ showt emoji)
      \(Entity (RoleKey rid) _) → do
        mr ← upgrade (gid, rid)
        fail . L.unpack $
          maybe "A role " (\r → "The role " <> r ^. #name) mr
          <> " already uses this emoji!"

    role ←
      invoke (CreateGuildRole gid $ ModifyGuildRoleData
        (Just roleName)
        Nothing
        Nothing
        (Just False)
        (Just False))
      >>= either (\_ → fail "Couldn't create role") pure
    reactTo (ctx ^. #message) $ namedEmoji "thumbsup"

    sqlPut' (RoleKey $ role ^. #id) $ Clam.Role (showt emoji) gk

addRolesCmd ∷ Cmd r
addRolesCmd = command_ @'[Text] "add-roles" \ctx grp → do
  gk ← groupFromName grp
  rs ← getRolesInGroup gk
    <&> M.mapMaybe (uncurry $ map . (,))
    <&> M.foldMapWithKey \_ (cr, dr) → M.singleton (cr ^. roleEmoji) dr

  Right myMsg ← tell ctx $
    "Select roles and click " <> showt applyEmoji <> " to apply.\n"
    <> "> " <> T.intercalate ", " (map (uncurry fmt) $ M.assocs rs)
  reactTo myMsg applyEmoji
  traverse_ (reactTo myMsg . UnicodeEmoji . toLazy) $ M.keys rs

  let uid = ctx ^. #user . #id
      weCareAboutIt msg rct =
           msg ^. #id == myMsg ^. #id
        && rct ^. #userID == uid
        && has (#emoji . #_UnicodeEmoji) rct

  reacts ← newIORef HS.empty

  la ← react @'MessageReactionAddEvt \(msg, rct) →
    when (weCareAboutIt msg rct) $
      modifyIORef' reacts $ HS.insert (showt $ rct ^. #emoji)
  lr ← react @'MessageReactionRemoveEvt \(msg, rct) →
    when (weCareAboutIt msg rct) $
      modifyIORef' reacts $ HS.delete (showt $ rct ^. #emoji)
  let removeListeners = la >> lr

  waitUntil @'MessageReactionAddEvt \(msg, rct) →
    msg ^. #id == myMsg ^. #id && rct ^. #emoji == applyEmoji
  invoke $ DeleteAllReactions myMsg myMsg
  removeListeners

  gid ← asks @Config $ view #guild
  readIORef reacts
    <&> HS.toList
    <&> mapMaybe (rs M.!?)
    >>= traverse_ (invoke . AddGuildMemberRole gid uid)
  void . reactTo myMsg $ namedEmoji "thumbsup"

  where
  applyEmoji = namedEmoji "white_check_mark"
  fmt emoji r = emoji <> ": " <> toStrict (r ^. #name)

getRolesInGroup ∷ Clam.BotC r ⇒
  Key Group →
  Sem r (Map (Snowflake D.Role) (Clam.Role, Maybe D.Role))
getRolesInGroup gk = do
  gid ← asks @Config $ view #guild
  (rids, cs) ← sqlSel [ RoleGroup ==. gk ]
    <&> unzip . map (unRoleKey . entityKey &&& entityVal)
  mds ← traverse (upgrade . (gid,)) rids
  pure $ M.fromList $ zip rids (zip cs mds)

groupFromName ∷ Members [Fail, Sql SqlBackend] r ⇒ Text → Sem r (Key Group)
groupFromName grp = sqlGetUq (UqGroupName grp)
  >>= maybe (fail "Group doesn't exist") (pure . entityKey)
