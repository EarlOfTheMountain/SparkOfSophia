{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import Data.StateVar (($=))
import qualified SDL

main :: IO ()
main = do
  SDL.initializeAll
  window <- SDL.createWindow "Spark of Sophia"
              SDL.defaultWindow { SDL.windowInitialSize = SDL.V2 800 600 }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  gameLoop renderer
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit

gameLoop :: SDL.Renderer -> IO ()
gameLoop renderer = do
  events <- SDL.pollEvents
  SDL.rendererDrawColor renderer $= SDL.V4 20 20 60 255
  SDL.clear renderer
  SDL.present renderer
  unless (any isQuit events) (gameLoop renderer)
  where
    isQuit e = SDL.eventPayload e == SDL.QuitEvent

