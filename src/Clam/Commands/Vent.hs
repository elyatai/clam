module Clam.Commands.Vent (commands) where

import Clam.Prelude
import Clam.Utils.Calamity
import Clam.Sql
import Clam.Types as Clam

import Calamity.Types hiding (Member)
import Calamity.Commands hiding (commands)

commands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
commands = void do
  command @'[Maybe GuildChannel] "set-vent" \ctx mchan → do
    chan ← channelOrHere ctx mchan
    sqlPut' (VentKey $ getID chan) $ Vent Nothing
    void . reactTo (ctx ^. #message) $ namedEmoji "thumbsup"

  command @'[Maybe GuildChannel] "unset-vent" \ctx mchan → do
    chan ← channelOrHere ctx mchan
    sqlDel $ VentKey $ getID chan
    void . reactTo (ctx ^. #message) $ namedEmoji "thumbsup"

channelOrHere ∷ Member Fail r ⇒
  Context → Maybe GuildChannel → Sem r GuildChannel
channelOrHere ctx mchan =
  whenNothing mchan $
    whenNothing (ctx ^? #channel . #_GuildChannel') $
      fail "You must be in a server to use this command!"
