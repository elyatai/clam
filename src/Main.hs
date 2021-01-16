module Main (main) where

import Clam.Prelude
import Clam.Config (Config)
import Clam.Persist
import Clam.Rdb

import Calamity hiding (Member, embed)
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import Control.Monad.Logger (NoLoggingT(runNoLoggingT))
import qualified Data.Text.Lazy as L
import Data.Time (getCurrentTime)
import Database.Persist.Postgresql
  ((=.), runSqlConn, runMigration, Entity(..), SqlBackend, withPostgresqlConn)
import Dhall (inputFile, auto)
import qualified Di
import DiPolysemy (runDiToIO)
import Polysemy.Fail (Fail)
import Relude.Unsafe (fromJust)
import Text.Emoji (emojiFromAlias)

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

  react @'MessageCreateEvt \msg → do

    -- reply to commands if invoked
    whenJust ((conf ^. #prefix2) `L.stripPrefix` (msg ^. #content)) \s →
      whenJustM (rdbGetUq $ UniqueCommand $ L.toStrict s) \(Entity _ cmd) →
        void $ tell @Text msg $ cmd ^. commandReply

    -- update vent timers if necessary
    whenJust (msg ^. #guildID) \_ → do
      mchan ← upgrade $ msg ^. #channelID
      whenJust (mchan >>= preview #_GuildChannel') \gchan → do
        now ← embed getCurrentTime
        rdbUpd (VentKey $ getID gchan) [VentLastMsg =. Just now]

  addCommands do
    command @'[] "ping" \ctx →
      void $ tell @Text ctx "pong"

    command @'[Text, KleenePlusConcat Text] "add-command" \ctx cmd reply → do
      res ← rdbPutUq $ Command cmd reply
      void . tell ctx $ case res of
        Left _ → "That command already exists!"
        Right _ → "Added command " <> cmd <> "!"

    command @'[Text] "del-command" \ctx cmd → do
      let uq = UniqueCommand cmd
      exists ← isJust <$> rdbGetUq uq
      rdbDelUq uq
      void . tell @Text ctx $
        if exists
        then "Command deleted!"
        else "Command didn't exist, nothing done"

    command @'[] "fail" \_ → fail "failed"

    command @'[Maybe GuildChannel] "set-vent" \ctx mchan → do
      chan ← channelOrHere ctx mchan
      rdbPut' (VentKey $ getID chan) $ Vent Nothing
      let msg = ctx ^. #message
      void . invoke $ CreateReaction msg msg $ namedEmoji "thumbsup"

    command @'[Maybe GuildChannel] "unset-vent" \ctx mchan → do
      chan ← channelOrHere ctx mchan
      rdbDel $ VentKey $ getID chan
      let msg = ctx ^. #message
      void . invoke $ CreateReaction msg msg $ namedEmoji "thumbsup"

  react @('CustomEvt "command-error" (Context, CommandError)) \(ctx, err) →
    void . tell ctx $ case err of
      ParseError _ty why → codeblock' Nothing why
      CheckError chk why → "Check " <> toLazy chk <> " failed: " <> why
      InvokeError _cmd why → "Error: " <> why

namedEmoji ∷ Text → RawEmoji
namedEmoji = UnicodeEmoji . toLazy . fromJust . emojiFromAlias

channelOrHere ∷ Member Fail r ⇒
  Context → Maybe GuildChannel → Sem r GuildChannel
channelOrHere ctx mchan =
  whenNothing mchan $
    whenNothing (ctx ^? #channel . #_GuildChannel') $
      fail "You must be in a server to use this command!"
