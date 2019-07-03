{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Presentation where

import Anim

import Data.Functor.Identity
import Data.Functor.Const
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (maybeToList)

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
  , _ranims :: [RAnim Presentation]
  , _scale :: Float
  , _slides :: [Slide]
  , _slidePointer :: Int
  , _imgCache :: Map String Picture
  , _indicatorLine :: Sprite Identity
  , _indicatorPoint :: Sprite Identity
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
    _ -> error "picture should be converted to gloss picture on creation, not during rendering"

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

prevSlideAnim :: Int -> Int -> Maybe Slide -> Anim Presentation
prevSlideAnim slideCount nextPointer slide = let
  nextIndicatorX = (-100) + (200 * (fromIntegral nextPointer / fromIntegral slideCount))
  in Seq $
  [ Par
    [ Base 0.5 (current . sprites . traverse . spriteX) (To (500))
    , Base 0.5 (current . sprites . traverse . spriteAlpha) (To 0)
    , Base 0.5 (indicatorPoint . spriteX) (To nextIndicatorX)
    ]
  , Par
    [ Set 0 (current . sprites) (\_ -> [])
    , Seq
      [ Base 0.1 (indicatorPoint . spriteScale) (To 2.5)
      , Base 0.05 (indicatorPoint . spriteScale) (To 1.8)
      , Base 0.05 (indicatorPoint . spriteScale) (To 2)
      ]
    ]
  , Seq (maybeToList slide)
  ]

nextSlideAnim :: Int -> Int -> Maybe Slide -> Anim Presentation
nextSlideAnim slideCount nextPointer slide = let
  nextIndicatorX = (-100) + (200 * (fromIntegral nextPointer / fromIntegral slideCount))
  in Seq $
  [ Par
    [ Base 0.5 (current . sprites . traverse . spriteX) (To (-500))
    , Base 0.5 (current . sprites . traverse . spriteAlpha) (To 0)
    , Base 0.5 (indicatorPoint . spriteX) (To nextIndicatorX)
    ]
  , Par
    [ Set 0 (current . sprites) (\_ -> [])
    , Seq
      [ Base 0.1 (indicatorPoint . spriteScale) (To 2.5)
      , Base 0.05 (indicatorPoint . spriteScale) (To 1.8)
      , Base 0.05 (indicatorPoint . spriteScale) (To 2)
      ]
    ]
  , Seq (maybeToList slide)
  ]

-- main functions

draw :: Presentation -> Picture
draw p = let
  currentSprites = p ^. current . sprites
  in Pictures (map drawSprite (currentSprites ++ [p ^. indicatorLine, p ^. indicatorPoint]))

handleInput :: Event -> Presentation -> Presentation
handleInput (EventKey (SpecialKey KeyRight) Down _ _) p@(Presentation {_ranims, _slides, _slidePointer}) = let
  nextPointer = _slidePointer + 1
  slideCount = length _slides
  newAnim = if nextPointer < slideCount
    then mkRAnim (nextSlideAnim slideCount nextPointer (Just (_slides !! nextPointer)))
    else mkRAnim (nextSlideAnim slideCount nextPointer (Nothing))
  -- all old animations are cleared when a move key is pressed
  in p { _slidePointer = nextPointer, _ranims = newAnim }
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) p@(Presentation {_ranims, _slides, _slidePointer}) = let
  nextPointer = _slidePointer - 1
  slideCount = length _slides
  newAnim = if nextPointer >= 0
    then mkRAnim (prevSlideAnim slideCount nextPointer (Just (_slides !! nextPointer)))
    else mkRAnim (prevSlideAnim slideCount nextPointer (Nothing))
  -- all old animations are cleared when a move key is pressed
  in p { _slidePointer = nextPointer, _ranims = newAnim }
handleInput _ p = p

update :: Float -> Presentation -> Presentation
update t p@(Presentation {_ranims}) = let
  (newPresentation, newAnims) = applyAnims p t _ranims
  in newPresentation { _ranims = newAnims }

initial :: (Map String Picture) -> Presentation
initial cache = let
  indicatorLine = Sprite (-100) (-200) 20 1 (GlossPic (Line [(0, 0), (10, 0)])) (Identity (-1))
  indicatorPoint = Sprite (-100) (-200) 2 1 (GlossPic (ThickCircle 1 3)) (Identity (-2))
  in Presentation (ActiveSlide []) (mkRAnim slide1) 1 [slide1, slide2, slide2, slide2] 0 cache indicatorLine indicatorPoint
