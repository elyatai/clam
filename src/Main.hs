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

  _ ← react @'ReadyEvt \rd → do
    let u = rd ^. #user
    info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator

  _ ← react @'MessageCreateEvt \msg →
    whenJust ((conf ^. #prefix2) `L.stripPrefix` (msg ^. #content)) \s →
      whenJustM (rdbGetUq $ UniqueCommand $ L.toStrict s) \(Entity _ cmd) →
        void $ tell @Text msg $ cmd ^. commandReply

  addCommands do
    command @'[] "ping" \ctx →
      void $ tell @Text ctx "pong"

    command @'[Text, KleenePlusConcat Text] "add-command" \ctx name reply → do
      res ← rdbPutUq $ Command name reply
      void . tell @Text ctx $ case res of
        Left _ → "That command already exists!"
        Right _ → "Added command " <> name <> "!"

    command @'[Text] "del-command" \ctx name → do
      exists ← isJust <$> rdbGetUq (UniqueCommand name)
      _ ← rdbDelUq $ UniqueCommand name
      void . tell @Text ctx $
        if exists
        then "Command deleted!"
        else "Command didn't exist, nothing done"
