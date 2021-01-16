module Main (main) where

import Clam.Prelude
import Clam.Config (Config)
import Clam.Rdb

import Calamity hiding (Member)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import qualified Data.Text.Lazy as L
import Database.Persist.Postgresql (withPostgresqlConn)
import Dhall (inputFile, auto)
import qualified Di
import DiPolysemy (runDiToIO)

main ∷ IO ()
main = Di.new \di → do
  conf ← inputFile @Config auto "./config.dhall"

  runNoLoggingT $ withPostgresqlConn (conf ^. #db) \db → liftIO do
    res ← runFinal . embedToFinal . runRdbConn db
      . runCacheInMemory . runMetricsNoop . runDiToIO di
      . useConstantPrefix (conf ^. #prefix)
      . runReader conf
      $ runBotIO (conf ^. #token) defaultIntents Main.bot

    whenJust res \(StartupError s) →
      Di.runDiT di $ Di.alert $ "Startup error: " <> s

bot ∷ (BotC r, Members '[ParsePrefix, Reader Config] r) ⇒ Sem r ()
bot = void do
  conf ← ask @Config

  react @'ReadyEvt \rd → do
    let u = rd ^. #user
    info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator

  react @'MessageCreateEvt \msg →
    whenJust ((conf ^. #prefix2) `L.stripPrefix` (msg ^. #content)) \s →
      _

  addCommands do
    command @'[] "ping" \ctx →
      void $ tell @Text (ctx ^. #message) "pong"
