module Dappish where

import Dappish.Prelude
import Dappish.Parser
import Dappish.Grok
import Dappish.Solidify

main :: IO ()
main = do
  x <- parseFromFile "dcs.dapp"
  case x of
    Just y -> do
      print y
      print (grok y)
    Nothing ->
      pure ()

dcs :: IO ()
dcs = do
  Just x <- parseFromFile "dcs.dapp"
  Right y <- pure (grok x)
  putStrLn (unpack (solidify y))
