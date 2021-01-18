{-# LANGUAGE AllowAmbiguousTypes #-}
module Clam.Utils.Calamity where

import Clam.Prelude
import Clam.Types as Clam
import Relude.Unsafe (fromJust)

import Calamity hiding (Member)
import Calamity.Commands (command, Context, DSLState)
import Calamity.Commands.CommandUtils (CommandForParsers)
import Text.Emoji (emojiFromAlias)

type Cmd r = Clam.BotC r ⇒ Sem (DSLState r) ()

command_ ∷ ∀ ps r. TypedCommandC ps r ⇒
  Text → (Context → CommandForParsers ps r) → Cmd r
command_ n = void . command n

reactTo ∷ (Calamity.BotC r, HasID Channel a, HasID Message a) ⇒
  a → RawEmoji → Sem r (Either RestError ())
reactTo t e = invoke $ CreateReaction t t e

namedEmoji ∷ Text → RawEmoji
namedEmoji = UnicodeEmoji . toLazy . fromJust . emojiFromAlias
