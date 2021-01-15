module Main (main) where
import Clam.Prelude

import Calamity
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import Dhall
import qualified Di
import DiPolysemy (info, runDiToIO)
import Polysemy

instance FromDhall Token where
  autoWith = fmap BotToken . autoWith

data Config = Config
  { token ∷ Token
  } deriving Generic
    deriving anyclass FromDhall

main ∷ IO ()
main = Di.new \di → do
  conf ← inputFile @Config auto "./config.dhall"
  res ← runFinal . embedToFinal
    . runCacheInMemory . runMetricsNoop . runDiToIO di
    . useConstantPrefix ";"
    $ runBotIO (conf ^. #token) defaultIntents do
      react @'ReadyEvt \rd → do
        let u = rd ^. #user
        info $ "Ready as " <> u ^. #username <> "#" <> u ^. #discriminator
  whenJust res \(StartupError s) →
    Di.runDiT di $ Di.alert $ "Startup error: " <> s

