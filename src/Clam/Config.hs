module Clam.Config where

import Clam.Prelude

import Calamity.Types
import qualified Data.Text.Lazy as L
import Dhall

instance FromDhall Token where
  autoWith = fmap BotToken . autoWith

instance FromDhall (Snowflake a) where
  autoWith _ = fmap (Snowflake . fromIntegral) natural

data Config = Config
  { token ∷ Token
  , prefix ∷ L.Text
  , guild ∷ Snowflake Guild
  } deriving Generic
    deriving anyclass FromDhall
