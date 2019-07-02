{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module World where

import Anim

import Data.Functor.Identity
import Data.Functor.Const

import Lens.Micro
import Lens.Micro.TH

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Sprite f
  = Sprite
  { _spriteX :: Float
  , _spriteY :: Float
  , _spriteScale :: Float
  , _spriteAlpha :: Float
  , _spriteIndex :: f Int
  , _spriteAlive :: Bool
  , _spritePicture :: Picture
  }

makeLenses ''Sprite

data World
  = World
  { _sprites :: [Sprite Identity]
  , _nextSpriteIndex :: Int
  , _ranims :: [RAnim World]
  , _scaleX :: Float
  , _scaleY :: Float
  }

makeLenses ''World

drawSprite :: Sprite f -> Picture
drawSprite (Sprite {_spriteX, _spriteY, _spriteAlpha, _spriteScale, _spritePicture}) =
  _spritePicture &
  Color (makeColor 1 1 1 _spriteAlpha) &
  Scale _spriteScale _spriteScale &
  Translate _spriteX _spriteY

box :: Picture
box = Pictures
  [ Line [(-1, 1), (1, 1)]
  , Line [(1, 1), (1, -1)]
  , Line [(1, -1), (-1, -1)]
  , Line [(-1, -1), (-1, 1)]
  ]

createSprite :: World -> (World, Int)
createSprite w@(World {_sprites}) = let
  newIndex = length _sprites
  newSprite = Sprite ((-200) + ((fromIntegral newIndex) * 80)) 0 30 0 (Identity newIndex) True box
  in (w { _sprites = _sprites ++ [newSprite] }, newIndex)

initialAnim :: [RAnim World]
initialAnim = let
  introAnim index = Par [Base 0.1 (sprites . ix' index . spriteAlpha) (To 1.0), Base 0.1 (sprites . ix' index . spriteY) (To (-10.0))]
  in mkRAnim $
  Create 7 createSprite $ \l ->
  Seq (map introAnim l)

jump :: [RAnim World]
jump = let
  jumpAnim index = Seq [Base 0.1 (sprites . ix' index . spriteY) (To 0.0), Base 0.1 (sprites . ix' index . spriteY) (To (-10.0))]
  in mkRAnim $
  Seq (map jumpAnim [0..6])

initialWorld :: World
initialWorld = World [] 0 initialAnim 1 1

draw :: World -> Picture
draw (World {_sprites, _scaleX, _scaleY}) = Pictures ((map drawSprite _sprites) ++ [headerText])
  & Scale _scaleX _scaleY

headerText :: Picture
headerText = let
  t (x,y) =  Text "Test" &
    Color white &
    Scale 0.25 0.25 &
    Translate x y
  in Pictures (map t [(-50, 50), (-49.5, 50), (-50.5, 50), (-50, 49.5), (-50, 50.5)])

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w@(World {_ranims}) = let
  newRAnims = _ranims ++ jump
  in w { _ranims = newRAnims }
handleInput (EventKey (Char '=') Down _ _) w@(World {_scaleX, _scaleY}) =
  w { _scaleX = _scaleX + 0.2, _scaleY = _scaleY + 0.2 }
handleInput (EventKey (Char '-') Down _ _) w@(World {_scaleX, _scaleY}) =
  w { _scaleX = _scaleX - 0.2, _scaleY = _scaleY - 0.2 }
handleInput _ w = w

update :: Float -> World -> World
update t w@(World {_ranims}) = let
  (newWorld, newAnims) = applyAnims w t _ranims
  in newWorld { _ranims = newAnims }
