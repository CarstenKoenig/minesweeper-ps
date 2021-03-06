module Main where

import Prelude

import Components.Game (game)
import Effect (Effect)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Models.Game as Game


main :: Effect Unit
main = do
  -- initialize a random game (size: 9x9 with 10 mines)
  gameState <- Game.initialize 9 9 10
  runHalogenAff do
    -- get a handle to the body after it's available
    body <- awaitBody
    -- run the game-component with the random game as input
    -- into the body of the browser-document
    runUI game gameState body