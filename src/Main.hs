{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Anim
import World

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = play (InWindow "test" (800, 200) (10, 10)) black 60 initialWorld draw handleInput update

draw :: World -> Picture
draw (World {_sprites}) = Pictures (map drawSprite _sprites)

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w@(World {_ranims}) = let
  newRAnims = _ranims ++ jump
  in w { _ranims = newRAnims }
handleInput _ w = w

update :: Float -> World -> World
update t w@(World {_ranims}) = let
  (newWorld, newAnims) = applyAnims w t _ranims
  in newWorld { _ranims = newAnims }

