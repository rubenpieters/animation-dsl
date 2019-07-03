{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Presentation where

import Anim

import Data.Functor.Identity
import Data.Functor.Const
import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro
import Lens.Micro.TH

data SpritePic = GlossPic Picture | FromCache String

data Sprite f
  = Sprite
  { _spriteX :: Float
  , _spriteY :: Float
  , _spriteScale :: Float
  , _spriteAlpha :: Float
  , _spritePicture :: SpritePic
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
  , _imgCache :: Map String Picture
  }

makeLenses ''Presentation

drawSprite :: Sprite f -> Picture
drawSprite (Sprite {_spriteX, _spriteY, _spriteAlpha, _spriteScale, _spritePicture}) =
  case _spritePicture of
    (GlossPic pic) ->
       pic &
       Color (makeColor 1 1 1 _spriteAlpha) &
       Scale _spriteScale _spriteScale &
       Translate _spriteX _spriteY

instantiateSpriteTemplate :: Sprite (Const ()) -> Int -> Map String Picture -> Sprite Identity
instantiateSpriteTemplate sprite index imgCache = let
  newPic = case sprite ^. spritePicture of
    (GlossPic pic) -> GlossPic pic
    (FromCache cacheId) -> case Map.lookup cacheId imgCache of
      (Just pic) -> GlossPic pic
      Nothing -> error ("can not find img " ++ show cacheId)
  in sprite { _spriteIndex = Identity index, _spritePicture = newPic }

createSprite :: Sprite (Const ()) -> Presentation -> (Presentation, Int)
createSprite template p = let
  newIndex = p ^. current . sprites & length
  newPresentation = p & current . sprites . ix' newIndex .~ instantiateSpriteTemplate template newIndex (p ^. imgCache)
  in (newPresentation, newIndex)

thick :: Picture -> Picture
thick picture = let
  f (x,y) = picture & Translate x y
  in Pictures (map f [(0, 0), (-0.5, 0), (0.5, 0), (0, -0.5), (0, 0.5)])

slide1 :: Slide
slide1 =
  -- Create 1 (createSprite (Sprite 0 0 0.01 1 (GlossPic (thick (Text "test"))) (Const ()))) $ \[t0] ->
  Create 1 (createSprite (Sprite 0 0 0.01 1 (FromCache "kulLogo") (Const ()))) $ \[t0] ->
  Base 0.3 (current . sprites . ix' t0 . spriteScale) (To 0.5)

slide2 :: Slide
slide2 =
  Create 1 (createSprite (Sprite 0 (-20) 0.3 1 (GlossPic (thick (Text "alpha"))) (Const ()))) $ \[t0] ->
  Create 1 (createSprite (Sprite 0 20 0.3 1 (GlossPic (thick (Text "alpha"))) (Const ()))) $ \[t1] ->
  Seq
  [ Base 0.5 (current . sprites . ix' t0 . spriteY) (To (-40))
  , Base 0.5 (current . sprites . ix' t1 . spriteY) (To (40))
  ]

nextSlideAnim :: Maybe Slide -> Anim Presentation
nextSlideAnim slide =
  Seq $
  [ Base 0.7 (current . sprites . traverse . spriteX) (To (-200))
  , Par
    [ Set 0 (current . sprites) (\_ -> [])
    , Set 0 (slidePointer) (\p -> p ^. slidePointer + 1)
    ]
  ] ++ case slide of
          (Just s) -> [s]
          Nothing -> []

-- main functions

draw :: Presentation -> Picture
draw p = let
  currentSprites = p ^. current . sprites
  nextSprites = p ^. current . sprites
  in Pictures (map drawSprite (currentSprites ++ nextSprites))

handleInput :: Event -> Presentation -> Presentation
handleInput (EventKey (SpecialKey KeyRight) Down _ _) p@(Presentation {_ranims, _slides, _slidePointer}) = let
  nextPointer = _slidePointer + 1
  newAnim = if nextPointer < length _slides
    then mkRAnim (nextSlideAnim (Just (_slides !! nextPointer)))
    else mkRAnim (nextSlideAnim (Nothing))
  newRAnims = _ranims ++ newAnim
  in p { _ranims = newRAnims }
handleInput _ p = p

update :: Float -> Presentation -> Presentation
update t p@(Presentation {_ranims}) = let
  (newPresentation, newAnims) = applyAnims p t _ranims
  in newPresentation { _ranims = newAnims }

initial :: (Map String Picture) -> Presentation
initial cache = Presentation (ActiveSlide []) (ActiveSlide []) (mkRAnim slide1) 1 [slide1, slide2] 0 cache
