{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Presentation where

import Anim

import Data.Functor.Identity
import Data.Functor.Const

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro
import Lens.Micro.TH

data Sprite f
  = Sprite
  { _spriteX :: Float
  , _spriteY :: Float
  , _spriteScale :: Float
  , _spriteAlpha :: Float
  , _spritePicture :: Picture
  , _spriteIndex :: f Int
  }

makeLenses ''Sprite

data ActiveSlide
  = ActiveSlide
  { _sprites :: [Sprite Identity]
  }

makeLenses ''ActiveSlide

type Slide = Anim Presentation

data Presentation
  = Presentation
  { _current :: ActiveSlide
  , _next :: ActiveSlide
  , _ranims :: [RAnim Presentation]
  , _scale :: Float
  , _slides :: [Slide]
  , _slidePointer :: Int
  }

makeLenses ''Presentation

drawSprite :: Sprite f -> Picture
drawSprite (Sprite {_spriteX, _spriteY, _spriteAlpha, _spriteScale, _spritePicture}) =
  _spritePicture &
  Color (makeColor 1 1 1 _spriteAlpha) &
  Scale _spriteScale _spriteScale &
  Translate _spriteX _spriteY

instantiateSpriteTemplate :: Sprite (Const ()) -> Int -> Sprite Identity
instantiateSpriteTemplate sprite index =
  sprite { _spriteIndex = Identity index }

createSprite :: Sprite (Const ()) -> Presentation -> (Presentation, Int)
createSprite template p = let
  newIndex = p ^. current . sprites & length
  newPresentation = p & current . sprites . ix' newIndex .~ instantiateSpriteTemplate template newIndex
  in ( newPresentation , newIndex )

thick :: Picture -> Picture
thick picture = let
  f (x,y) = picture & Translate x y
  in Pictures (map f [(0, 0), (-0.5, 0), (0.5, 0), (0, -0.5), (0, 0.5)])

slide1 :: Slide
slide1 =
  Create 1 (createSprite (Sprite 0 0 0.01 1 (thick (Text "test")) (Const ()))) $ \[t0] ->
  Base 0.3 (current . sprites . ix' t0 . spriteScale) (To 0.5)

slide2 :: Slide
slide2 =
  Create 2 (createSprite (Sprite 0 0 0.1 0.5 (thick (Text "alpha")) (Const ()))) $ \[t0, t1] ->
  Seq
  [ Base 0.5 (current . sprites . ix' t0 . spriteAlpha) (To 1.0)
  , Base 0.5 (current . sprites . ix' t1 . spriteAlpha) (To 1.0)
  ]

-- main functions

draw :: Presentation -> Picture
draw p = let
  currentSprites = p ^. current . sprites
  nextSprites = p ^. current . sprites
  in Pictures (map drawSprite (currentSprites ++ nextSprites))

handleInput :: Event -> Presentation -> Presentation
handleInput (EventKey (SpecialKey KeyRight) Down _ _) p@(Presentation {_ranims}) = let
  newRAnims = _ranims ++ []
  in p { _ranims = newRAnims }
handleInput _ p = p

update :: Float -> Presentation -> Presentation
update t p@(Presentation {_ranims}) = let
  (newPresentation, newAnims) = applyAnims p t _ranims
  in newPresentation { _ranims = newAnims }

initial :: Presentation
initial = Presentation (ActiveSlide []) (ActiveSlide []) (mkRAnim slide1) 1 [] 0
