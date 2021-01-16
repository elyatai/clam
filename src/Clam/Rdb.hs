{-# LANGUAGE TemplateHaskell #-}
module Clam.Rdb
  ( Rdb(..), RdbC
  , rdbGet, rdbGetUq, rdbPut, rdbPut', rdbDel, rdbDelUq, rdbUpd
  , runRdbConn, runRdbPool
  ) where

import Clam.Prelude

import Data.Pool (Pool)
import Database.Persist
import Database.Persist.Sql

type PRB r b = PersistRecordBackend r b

data Rdb b m a where
  RdbGet   ∷ PRB r b ⇒ Key r               → Rdb b m (Maybe r)
  RdbGetUq ∷ PRB r b ⇒ Unique r            → Rdb b m (Maybe (Entity r))
  RdbPut   ∷ PRB r b ⇒ r                   → Rdb b m (Key r)
  RdbPut'  ∷ PRB r b ⇒ Key r → r          → Rdb b m ()
  RdbDel   ∷ PRB r b ⇒ Key r               → Rdb b m ()
  RdbDelUq ∷ PRB r b ⇒ Unique r            → Rdb b m ()
  RdbUpd   ∷ PRB r b ⇒ Key r → [Update r] → Rdb b m ()

makeSem ''Rdb

type RdbC backend =
  ( PersistStoreWrite backend, PersistUniqueWrite backend
  , BackendCompatible SqlBackend backend
  )

runRdbConn ∷ ∀ backend r a.
  (RdbC backend, Member (Embed IO) r) ⇒
  backend → Sem (Rdb backend ': r) a → Sem r a
runRdbConn conn = transform go . reinterpret (embed . runRdb1) where
  go ∷ ∀ m x. Embed (ReaderT backend IO) m x → Embed IO m x
  go = Embed . flip runSqlConn conn . unEmbed

runRdbPool ∷ ∀ backend r a.
  (RdbC backend, Member (Embed IO) r) ⇒
  Pool backend → Sem (Rdb backend ': r) a → Sem r a
runRdbPool pool = transform go . reinterpret (embed . runRdb1) where
  go ∷ ∀ m x. Embed (ReaderT backend IO) m x → Embed IO m x
  go = Embed . flip runSqlPool pool . unEmbed

runRdb1 ∷ RdbC backend ⇒
  Rdb backend m x → ReaderT backend IO x
runRdb1 = \case
  RdbGet k → get k
  RdbGetUq u → getBy u
  RdbPut x → insert x
  RdbPut' k x → insertKey k x
  RdbDel k → delete k
  RdbDelUq u → deleteBy u
  RdbUpd k us → update k us

