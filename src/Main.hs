module Main (main) where

import Clam.Prelude
import Clam.Config (Config)
import Clam.Persist
import Clam.Rdb

import Calamity hiding (Member)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import qualified Data.Text.Lazy as L
import Database.Persist.Postgresql (runSqlConn, runMigration, Entity(..), SqlBackend, withPostgresqlConn)
import Dhall (inputFile, auto)
import qualified Di
import DiPolysemy (runDiToIO)

main ∷ IO ()
main = Di.new \di → do
  conf ← inputFile @Config auto "./config.dhall"

  runNoLoggingT $ withPostgresqlConn (conf ^. #db) \db → liftIO do
    runSqlConn (runMigration migrateAll) db

    res ← runFinal . embedToFinal . runRdbConn db
      . runCacheInMemory . runMetricsNoop . runDiToIO di
      . useConstantPrefix (conf ^. #prefix)
      . runReader conf
      $ runBotIO (conf ^. #token) defaultIntents Main.bot

    whenJust res \(StartupError s) →
      Di.runDiT di $ Di.alert $ "Startup error: " <> s

type BotC r =
  ( Calamity.BotC r
  , Members '[ParsePrefix, Reader Config, Rdb SqlBackend] r
  )

bot ∷ Main.BotC r ⇒ Sem r ()
bot = void do
  conf ← ask @Config

  react @'ReadyEvt \rd → do
    let u = rd ^. #user
    info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator

  react @'MessageCreateEvt \msg →
    whenJust ((conf ^. #prefix2) `L.stripPrefix` (msg ^. #content)) \s →
      whenJustM (rdbGetUq $ UniqueCommand $ L.toStrict s) \(Entity _ cmd) →
        void $ tell @Text msg $ cmd ^. commandReply

  addCommands do
    command @'[] "ping" \ctx →
      void $ tell @Text (ctx ^. #message) "pong"
