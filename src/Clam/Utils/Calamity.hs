module Clam.Utils.Calamity where

import Clam.Prelude
import Relude.Unsafe (fromJust)

import Calamity
import Text.Emoji (emojiFromAlias)

reactTo ∷ (Calamity.BotC r, HasID Channel a, HasID Message a) ⇒
  a → RawEmoji → Sem r (Either RestError ())
reactTo t e = invoke $ CreateReaction t t e

namedEmoji ∷ Text → RawEmoji
namedEmoji = UnicodeEmoji . toLazy . fromJust . emojiFromAlias
