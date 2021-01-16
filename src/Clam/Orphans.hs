{-# OPTIONS_GHC -Wno-orphans #-}
module Clam.Orphans where
import Relude -- can't import Clam.Prelude here or cyclic dependency

import Calamity.Types (Snowflake(..), Token(..))
import qualified Data.ByteString.Char8 as BS8
import Database.Persist (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Dhall (string, FromDhall(..), natural)
import Web.HttpApiData (ToHttpApiData, FromHttpApiData)
import Web.PathPieces (PathPiece)

instance FromDhall Token where
  autoWith = fmap BotToken . autoWith

instance FromDhall (Snowflake a) where
  autoWith _ = fmap (Snowflake . fromIntegral) natural

instance FromDhall ByteString where
  autoWith _ = fmap BS8.pack string

deriving newtype instance PersistField (Snowflake a)
deriving newtype instance PersistFieldSql (Snowflake a)
-- needed to use as db key
deriving newtype instance Read (Snowflake a)
deriving newtype instance ToHttpApiData (Snowflake a)
deriving newtype instance FromHttpApiData (Snowflake a)
deriving newtype instance PathPiece (Snowflake a)
