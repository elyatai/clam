module Main (main) where
import Clam.Prelude

import Calamity
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import qualified Data.Text.Lazy as L
import Dhall hiding (void)
import qualified Di
import DiPolysemy (info, runDiToIO)
import Polysemy

instance FromDhall Token where
  autoWith = fmap BotToken . autoWith

data Config = Config
  { token ∷ Token
  , prefix ∷ L.Text
  } deriving Generic
    deriving anyclass FromDhall

main ∷ IO ()
main = Di.new \di → do
  conf ← inputFile @Config auto "./config.dhall"
  res ← runFinal . embedToFinal
    . runCacheInMemory . runMetricsNoop . runDiToIO di
    . useConstantPrefix (conf ^. #prefix)
    $ runBotIO (conf ^. #token) defaultIntents do

      react @'ReadyEvt \rd → do
        let u = rd ^. #user
        info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator

      addCommands do
        command @'[] "ping" \ctx →
          void $ tell @Text (ctx ^. #message) "pong"

  whenJust res \(StartupError s) →
    Di.runDiT di $ Di.alert $ "Startup error: " <> s
