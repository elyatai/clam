module Clam.Config where

import Clam.Prelude

import Calamity.Types
import qualified Data.Text.Lazy as L
import Dhall

data Config = Config
  { token   ∷ Token
  , prefix  ∷ L.Text -- bespoke commands
  , prefix2 ∷ L.Text -- user-defined text echo commands
  , guild   ∷ Snowflake Guild
  , db      ∷ ByteString
  } deriving Generic
    deriving anyclass FromDhall
