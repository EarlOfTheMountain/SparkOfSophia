{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (forM_, unless)
import Data.StateVar
import SDL (V2(..), V4(..))
import qualified SDL
import qualified SDL.Mixer as Mix

data Sounds = Sounds
  { sndAttack :: Mix.Chunk
  , sndDodge  :: Mix.Chunk
  , sndHeal   :: Mix.Chunk
  , sndMenu   :: Mix.Chunk
  }

main :: IO ()
main = do
  SDL.initializeAll
  Mix.openAudio Mix.defaultAudio 1024

  win <- SDL.createWindow "Spark of Sophia"
           SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }
  renderer <- SDL.createRenderer win (-1) SDL.defaultRenderer

  music  <- Mix.load "assets/audio/music/dungeon_ambient.ogg" :: IO Mix.Music
  sounds <- loadSounds

  Mix.playMusic Mix.Forever music
  loop renderer music sounds True

  Mix.free music
  freeSounds sounds
  Mix.closeAudio
  SDL.destroyRenderer renderer
  SDL.destroyWindow win
  SDL.quit

loadSounds :: IO Sounds
loadSounds = do
  let dir = "assets/audio/sfx/"
  Sounds
    <$> Mix.load (dir ++ "attack.ogg")
    <*> Mix.load (dir ++ "dodge.ogg")
    <*> Mix.load (dir ++ "heal.ogg")
    <*> Mix.load (dir ++ "menu.ogg")

freeSounds :: Sounds -> IO ()
freeSounds s = forM_ [sndAttack s, sndDodge s, sndHeal s, sndMenu s] Mix.free

loop :: SDL.Renderer -> Mix.Music -> Sounds -> Bool -> IO ()
loop renderer music sounds playing = do
  events <- SDL.pollEvents
  let keys = [k | e <- events, Just k <- [keyPress e]]
      quit = any isQuit events || SDL.KeycodeEscape `elem` keys

  playing' <- if SDL.KeycodeSpace `elem` keys
                then toggleMusic music playing
                else return playing

  forM_ keys (handleKey sounds)

  SDL.rendererDrawColor renderer $= V4 20 20 60 255
  SDL.clear renderer
  SDL.present renderer

  unless quit $ loop renderer music sounds playing'

toggleMusic :: Mix.Music -> Bool -> IO Bool
toggleMusic music playing
  | playing = Mix.haltMusic >> return False
  | otherwise = Mix.playMusic Mix.Forever music >> return True

handleKey :: Sounds -> SDL.Keycode -> IO ()
handleKey s k
  | k == SDL.KeycodeZ = Mix.play (sndAttack s)
  | k == SDL.KeycodeX = Mix.play (sndDodge s)
  | k == SDL.KeycodeC = Mix.play (sndHeal s)
  | k == SDL.KeycodeV = Mix.play (sndMenu s)
  | otherwise         = return ()

keyPress :: SDL.Event -> Maybe SDL.Keycode
keyPress e = case SDL.eventPayload e of
  SDL.KeyboardEvent ke
    | SDL.keyboardEventKeyMotion ke == SDL.Pressed
    , not (SDL.keyboardEventRepeat ke)
    -> Just (SDL.keysymKeycode (SDL.keyboardEventKeysym ke))
  _ -> Nothing

isQuit :: SDL.Event -> Bool
isQuit e = case SDL.eventPayload e of
  SDL.QuitEvent -> True
  _             -> False

