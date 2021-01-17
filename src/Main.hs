module Main (main) where

import Clam.Prelude
import Clam.Persist
import Clam.Types as Clam
import Clam.Rdb

import Clam.Commands.Commands as Commands
import Clam.Commands.Roles as Roles
import Clam.Commands.Vent as Vent

import Calamity hiding (Member, embed)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Commands.Check
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import qualified Data.Text.Lazy as L
import Data.Flags ((.>=.))
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
  ((=.), runSqlConn, runMigration, Entity(..), withPostgresqlConn)
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

bot ∷ Clam.BotC r ⇒ Sem r ()
bot = void do
  conf ← ask @Config

  react @'ReadyEvt \rd → do
    let u = rd ^. #user
    info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator

  react @'MessageCreateEvt \msg → do

    -- reply to commands if invoked
    whenJust ((conf ^. #prefix2) `L.stripPrefix` (msg ^. #content)) \s →
      whenJustM (rdbGetUq $ UqCommandName $ L.toStrict s) \(Entity _ cmd) →
        void $ tell @Text msg $ cmd ^. commandReply

    -- update vent timers if necessary
    whenJust (msg ^. #guildID) \_ → do
      mchan ← upgrade $ msg ^. #channelID
      whenJust (mchan >>= preview #_GuildChannel') \gchan → do
        now ← embed getCurrentTime
        rdbUpd (VentKey $ getID gchan) [VentLastMsg =. Just now]

  addCommands do
    command @'[] "ping" \ctx → void $ tell @Text ctx "pong"

    Commands.commands

    do chk ← mkPermsCheck "Manage Channels" manageChannels
       requires [chk] Vent.commands

    do chk ← mkPermsCheck "Manage Roles" manageRoles
       requires [chk] Roles.commands

  react @('CustomEvt "command-error" (Context, CommandError)) \(ctx, err) →
    void . tell ctx $ case err of
      ParseError _ty why → codeblock' Nothing why
      CheckError chk why → "Check " <> toLazy chk <> " failed: " <> why
      InvokeError _cmd why → "Error: " <> why

mkPermsCheck ∷ Calamity.BotC r ⇒ Text → Permissions → Sem r Check
mkPermsCheck s p = buildCheck (s <> " permissions") \ctx →
  case ctx ^. #guild of
    Nothing → pure $ Just "Not in a server"
    Just g →
      -- HACK: for some reason ctx ^. #member is Nothing
      invoke (GetGuildMember g $ ctx ^. #user) >>= \case
        Left e → alert (showt e) $> Just "Something went wrong"
        Right m → pure $
          if permissionsIn g m .>=. p
          then Nothing
          else Just "You don't have permission!"
