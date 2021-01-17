{-# LANGUAGE TemplateHaskell #-}
module Clam.Sql
  ( Sql(..), SqlC
  , sqlGet, sqlGetUq, sqlSel, sqlCount, sqlHas
  , sqlPut, sqlPut', sqlPutUq, sqlDel, sqlDelUq, sqlUpd
  , sqlGetQ, sqlDelQ, sqlUpdQ
  , runSqlConn, runSqlPool
  , P.SqlBackend
  ) where

import Clam.Prelude

import Data.Pool (Pool)
import Database.Persist
import qualified Database.Persist.Sql as P
import Database.Esqueleto.Internal.Internal as E

type PRB r b = PersistRecordBackend r b

type UpdateC val =
  ( PersistEntity val
  , BackendCompatible P.SqlBackend (PersistEntityBackend val)
  )

data Sql b m a where
  SqlGet   ∷ PRB r b ⇒ Key r → Sql b m (Maybe r)
  SqlGetUq ∷ PRB r b ⇒ Unique r → Sql b m (Maybe (Entity r))
  SqlSel   ∷ PRB r b ⇒ [Filter r] → Sql b m [Entity r]
  SqlCount ∷ PRB r b ⇒ [Filter r] → Sql b m Word
  SqlHas   ∷ PRB r b ⇒ [Filter r] → Sql b m Bool
  SqlPut   ∷ PRB r b ⇒ r → Sql b m (Key r)
  SqlPut'  ∷ PRB r b ⇒ Key r → r → Sql b m ()
  SqlPutUq ∷ (PRB r b, AtLeastOneUniqueKey r) ⇒
    r → Sql b m (Either (Entity r) (Key r))
  SqlDel   ∷ PRB r b ⇒ Key r → Sql b m ()
  SqlDelUq ∷ PRB r b ⇒ Unique r → Sql b m ()
  SqlUpd   ∷ PRB r b ⇒ Key r → [P.Update r] → Sql b m ()

  SqlGetQ  ∷ SqlSelect a r ⇒ SqlQuery a → Sql b m [r]
  SqlDelQ  ∷ SqlQuery () → Sql b m Word
  SqlUpdQ  ∷ UpdateC r ⇒ (SqlExpr (Entity r) → SqlQuery ()) → Sql b m Word

makeSem ''Sql

type SqlC backend =
  ( PersistStore backend, PersistUnique backend, PersistQuery backend
  , BackendCompatible P.SqlBackend backend
  )

runSqlConn ∷ ∀ backend r a.
  (SqlC backend, Member (Embed IO) r) ⇒
  backend → Sem (Sql backend ': r) a → Sem r a
runSqlConn conn = transform go . reinterpret (embed . runSql1) where
  go ∷ ∀ m x. Embed (ReaderT backend IO) m x → Embed IO m x
  go = Embed . flip P.runSqlConn conn . unEmbed

runSqlPool ∷ ∀ backend r a.
  (SqlC backend, Member (Embed IO) r) ⇒
  Pool backend → Sem (Sql backend ': r) a → Sem r a
runSqlPool pool = transform go . reinterpret (embed . runSql1) where
  go ∷ ∀ m x. Embed (ReaderT backend IO) m x → Embed IO m x
  go = Embed . flip P.runSqlPool pool . unEmbed

runSql1 ∷ SqlC backend ⇒ Sql backend m x → ReaderT backend IO x
runSql1 = \case
  SqlGet k → get k
  SqlGetUq u → getBy u
  SqlSel fs → selectList fs []
  SqlCount fs → fromIntegral <$> P.count fs
  SqlHas fs → (/= 0) <$> P.count fs
  SqlPut x → insert x
  SqlPut' k x → insertKey k x
  SqlPutUq x → insertBy x
  SqlDel k → P.delete k
  SqlDelUq u → deleteBy u
  SqlUpd k us → P.update k us

  SqlGetQ q → E.select q
  SqlDelQ q → fromIntegral <$> E.deleteCount q
  SqlUpdQ f → fromIntegral <$> E.updateCount f
