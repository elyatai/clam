{-# LANGUAGE TemplateHaskell #-}
module Clam.Rdb
  ( Rdb(..), RdbC
  , rdbGet, rdbGetUq, rdbSel, rdbCount, rdbHas
  , rdbPut, rdbPut', rdbPutUq
  , rdbDel, rdbDelUq
  , rdbUpd
  , runRdbConn, runRdbPool
  , SqlBackend
  ) where

import Clam.Prelude

import Data.Pool (Pool)
import Database.Persist
import Database.Persist.Sql

type PRB r b = PersistRecordBackend r b

data Rdb b m a where
  RdbGet   ∷ PRB r b ⇒ Key r → Rdb b m (Maybe r)
  RdbGetUq ∷ PRB r b ⇒ Unique r → Rdb b m (Maybe (Entity r))
  RdbSel   ∷ PRB r b ⇒ [Filter r] → Rdb b m [Entity r]
  RdbCount ∷ PRB r b ⇒ [Filter r] → Rdb b m Word
  RdbHas   ∷ PRB r b ⇒ [Filter r] → Rdb b m Bool
  RdbPut   ∷ PRB r b ⇒ r → Rdb b m (Key r)
  RdbPut'  ∷ PRB r b ⇒ Key r → r → Rdb b m ()
  RdbPutUq ∷ (PRB r b, AtLeastOneUniqueKey r) ⇒
    r → Rdb b m (Either (Entity r) (Key r))
  RdbDel   ∷ PRB r b ⇒ Key r → Rdb b m ()
  RdbDelUq ∷ PRB r b ⇒ Unique r → Rdb b m ()
  RdbUpd   ∷ PRB r b ⇒ Key r → [Update r] → Rdb b m ()

makeSem ''Rdb

type RdbC backend =
  ( PersistStore backend, PersistUnique backend, PersistQuery backend
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
  RdbSel fs → selectList fs []
  RdbCount fs → fromIntegral <$> count fs
  RdbHas fs → (/= 0) <$> count fs
  RdbPut x → insert x
  RdbPut' k x → insertKey k x
  RdbPutUq x → insertBy x
  RdbDel k → delete k
  RdbDelUq u → deleteBy u
  RdbUpd k us → update k us
