module Main where

import Prelude

import Effect (Effect)
import Effect.Print (print)

main :: Effect Unit
main = do
  let a = 5
      b = 6
  print $ "Eleven: " <> show (5 + 6)
