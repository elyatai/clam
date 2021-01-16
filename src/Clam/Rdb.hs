{-# LANGUAGE TemplateHaskell #-}
module Clam.Rdb where

import Clam.Prelude

import Database.Persist

data Rdb r b m a where
  RdbGet   ∷ Key r               → Rdb r b m (Maybe r)
  RdbGetUq ∷ Unique r            → Rdb r b m (Maybe (Entity r))
  RdbPut   ∷ r                   → Rdb r b m (Key r)
  RdbPut'  ∷ Key r → r          → Rdb r b m ()
  RdbDel   ∷ Key r               → Rdb r b m ()
  RdbDelUq ∷ Unique r            → Rdb r b m ()
  RdbUpd   ∷ Key r → [Update r] → Rdb r b m ()

makeSem ''Rdb

runRdb ∷ ∀ record backend r a.
  ( PersistRecordBackend record backend
  , PersistStoreWrite backend, PersistUniqueWrite backend
  , Member (Embed IO) r
  ) ⇒ backend → Sem (Rdb record backend ': r) a → Sem r a
runRdb backend = interpret (go >>> flip runReaderT backend >>> liftIO) where
  go ∷ ∀ m x. Rdb record backend m x → ReaderT backend IO x
  go = \case
    RdbGet k → get k
    RdbGetUq u → getBy u
    RdbPut x → insert x
    RdbPut' k x → insertKey k x
    RdbDel k → delete k
    RdbDelUq u → deleteBy u
    RdbUpd k us → update k us
