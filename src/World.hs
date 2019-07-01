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
  }

makeLenses ''World

ix' :: Int -> Lens' [a] a
ix' i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

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
initialWorld = World [] 0 initialAnim
