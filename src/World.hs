{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module World where

import Prelude hiding (seq)

import AnimEff

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
  , _ranims :: [Anim' World]
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


initialAnim :: Anim' World
initialAnim = let
  introAnim index =
    par
    [ Bind (Base 0.1 (sprites . ix' index . spriteAlpha) (To 1.0)) (Return)
    , Bind (Base 0.1 (sprites . ix' index . spriteY) (To (-10.0))) (Return)
    ]
  in
  Bind (Create 7 createSprite) $ \l ->
  seq (map introAnim l)

jump :: Anim' World
jump = let
  jumpAnim index =
    seq
    [ Bind (Base 0.1 (sprites . ix' index . spriteY) (To 0.0)) (Return)
    , Bind (Base 0.1 (sprites . ix' index . spriteY) (To (-10.0))) (Return)
    ]
  in
  seq (map jumpAnim [0..6])

initialWorld :: World
initialWorld = World [] 0 [initialAnim] 1 1

draw :: World -> Picture
draw w@(World {_sprites, _scaleX, _scaleY}) = let
  value = w ^? sprites . ix 0 . spriteAlpha
  in Pictures ((map drawSprite _sprites) ++ [headerText (show value)])
  & Scale _scaleX _scaleY

headerText :: String -> Picture
headerText text = let
  t (x,y) =  Text text &
    Color white &
    Scale 0.25 0.25 &
    Translate x y
  in Pictures (map t [(-50, 50), (-49.5, 50), (-50.5, 50), (-50, 49.5), (-50, 50.5)])

handleInput :: Event -> World -> World
handleInput (EventKey (Char 'x') Down _ _) w@(World {_ranims}) = let
  newRAnims = _ranims ++ [jump]
  in w { _ranims = newRAnims }
handleInput (EventKey (Char '=') Down _ _) w@(World {_scaleX, _scaleY}) =
  w { _scaleX = _scaleX + 0.2, _scaleY = _scaleY + 0.2 }
handleInput (EventKey (Char '-') Down _ _) w@(World {_scaleX, _scaleY}) =
  w { _scaleX = _scaleX - 0.2, _scaleY = _scaleY - 0.2 }
handleInput _ w = w

update :: Float -> World -> World
update t w@(World {_ranims}) = let
  (newWorld, newAnims) = applyAnims w t _ranims
  filteredAnims = filter f newAnims
  f (Return _) = False
  f _ = True
  in newWorld { _ranims = filteredAnims }

main :: IO ()
main = play (InWindow "test" (800, 200) (10, 10)) black 60 initialWorld draw handleInput update
