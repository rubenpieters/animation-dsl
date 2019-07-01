{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Anim where

import Data.List

import Lens.Micro

-- duration in s
type Duration = Float

data Anim obj where
  Seq :: [Anim obj] -> Anim obj
  Par :: [Anim obj] -> Anim obj
  Base :: Duration -> Lens' obj Float -> Tween Float -> Anim obj
  Create :: Int -> (obj -> (obj, Int)) -> ([Int] -> Anim obj) -> Anim obj

instance Show (Anim obj) where
  show (Seq anims) = "Seq (" ++ intercalate "," (map show anims) ++ ")"
  show (Par anims) = "Par (" ++ intercalate "," (map show anims) ++ ")"
  show (Base duration lens tween) = "Base " ++ show duration ++ " (s)"
  show (Create index _ f) = "Create " ++ show index ++ " (" ++ show (f []) ++ ")"

data Tween value
  = To value
  | From value
  deriving Show

data RAnim obj where
  OnFinish :: [RAnim obj] -> Anim obj -> RAnim obj
  RBase :: Duration -> Lens' obj Float -> Tween Float -> RAnim obj
  RCreate :: Int -> (obj -> (obj, Int)) -> ([Int] -> Anim obj) -> RAnim obj

instance Show (RAnim obj) where
  show (OnFinish running anim) = "OnFinish (" ++ intercalate "," (map show running) ++ ") (" ++ show anim ++ ")"
  show (RBase duration lens tween) = "RBase " ++ show duration ++ " (s)"
  show (RCreate index _ f) = "RCreate" ++ show (f [])

mkRAnim :: Anim obj -> [RAnim obj]
mkRAnim (Seq (anim:r)) = [OnFinish (mkRAnim anim) (Seq r)]
mkRAnim (Seq []) = []
mkRAnim (Par r) = foldMap mkRAnim r
mkRAnim (Base index lens tween) = [RBase index lens tween]
mkRAnim (Create amount create next) = [RCreate amount create next]

applyAnims :: obj -> Float -> [RAnim obj] -> (obj, [RAnim obj])
applyAnims obj t [] = (obj, [])
applyAnims obj t (anim:r) = let
  (obj', animsAfterAnim) = applyAnim obj t anim
  (obj'', animsAfterRest) = applyAnims obj' t r
  in (obj'', animsAfterAnim ++ animsAfterRest)

applyAnim :: obj -> Float -> RAnim obj -> (obj, [RAnim obj])
applyAnim obj t (RBase duration lens tween) = let
  -- update obj
  newObj = obj & lens %~ \current -> current + stepTween t duration tween current
  -- update anim
  newDuration = duration - t
  newAnim = if newDuration > 0
    then [RBase (duration - t) lens tween]
    else []
  in (newObj, newAnim)
applyAnim obj t (OnFinish [] next) = (obj, mkRAnim next)
applyAnim obj t (OnFinish anims next) = let
  (newObj, newAnims) = applyAnims obj t anims
  in (newObj, [OnFinish newAnims next])
applyAnim obj t (RCreate amount create next) = let
  (newObj, indexList) = batchCreate amount obj create
  in (newObj, mkRAnim (next indexList))

batchCreate :: Int -> obj -> (obj -> (obj, Int)) -> (obj, [Int])
batchCreate 0 obj create = (obj, [])
batchCreate amount obj create = let
  (prevObj, prevList) = batchCreate (amount - 1) obj create
  (newObj, newIndex) = create prevObj
  in (newObj, prevList ++ [newIndex])

stepTween ::
  Float -> -- time elapsed in s
  Float -> -- duration
  Tween Float -> -- tween target
  Float -> -- current value
  Float -- new value
stepTween t duration (To x) currentValue =
  (x - currentValue) * t / duration
stepTween t duration (From x) currentValue =
  error "TODO stepTween"
