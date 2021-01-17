module Clam.Commands.Commands (commands) where

import Clam.Prelude
import Clam.Rdb
import Clam.Types as Clam

import Calamity
import Calamity.Commands hiding (commands)

commands ∷ Clam.BotC r ⇒ Sem (DSLState r) ()
commands = void do
  command @'[Text, KleenePlusConcat Text] "add-command" \ctx cmd reply → do
    res ← rdbPutUq $ Command cmd reply
    void . tell ctx $ case res of
      Left _ → "That command already exists!"
      Right _ → "Added command " <> cmd <> "!"

  command @'[Text] "del-command" \ctx cmd → do
    let uq = UqCommandName cmd
    exists ← isJust <$> rdbGetUq uq
    rdbDelUq uq
    void . tell @Text ctx $
      if exists
      then "Command deleted!"
      else "Command didn't exist, nothing done"
