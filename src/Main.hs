module Main (main) where
import Clam.Prelude

import Calamity.Types
import Dhall

instance FromDhall Token where
  autoWith = fmap BotToken . autoWith

data Config = Config
  { token ∷ Token
  } deriving Generic
    deriving anyclass FromDhall

main ∷ IO ()
main = do
  conf ← inputFile @Config auto "./config.dhall"
  print (conf ^. #token)
