module Main (main) where

import Clam.Prelude
import Clam.Config

import Calamity
import Calamity.Cache.InMemory (runCacheInMemory)
import Calamity.Commands
import Calamity.Metrics.Noop (runMetricsNoop)
import Dhall (inputFile, auto)
import qualified Di
import DiPolysemy
import Polysemy

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
