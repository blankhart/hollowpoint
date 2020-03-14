module Main where

import Prelude

import Effect (Effect)
import Effect.Print (print)
import Other (_underscored)

main :: Effect Unit
main = do
  let a = 5
      b = 6
  print $ "Eleven: " <> show (5 + 6)
  print "Hello."
  print $ "Underscored: " <> show _underscored
