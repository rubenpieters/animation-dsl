{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuantifiedConstraints #-}

module AnimEff where

import Prelude hiding (seq)

import Lens.Micro

-- duration in s
type Duration = Float
-- delay in s
type Delay = Float

data Tween value
  = To value
  | From value
  deriving Show

data Anim obj a where
  Base :: Duration -> Traversal' obj Float -> Tween Float -> Anim obj ()
  Set :: Delay -> Traversal' obj value -> (obj -> value) -> Anim obj ()
  Create :: Int -> (obj -> (obj, Int)) -> Anim obj [Int]

instance Show (Anim obj a) where
  show (Base duration _ _) = "Base " ++ show duration
  show (Set delay _ _) = "Set " ++ show delay
  show (Create amount _) = "Create " ++ show amount

data Dsl f a where
  Bind :: f a -> (a -> Dsl f b) -> Dsl f b
  Return :: a -> Dsl f a
  Par :: [Dsl f a] -> ([a] -> Dsl f b) -> Dsl f b

instance (forall a. Show (f a)) => Show (Dsl f a) where
  show (Bind fa k) = "Bind (" ++ show fa ++ ") (...)"
  show (Par fs k) = "Par (" ++ show fs ++ ") (...)"
  show (Return a) = "Return"


--  Ap :: f a -> Dsl f (a -> b) -> Dsl f b

deriving instance Functor (Dsl f)

type Anim' obj = Dsl (Anim obj) ()

bind :: Dsl f a -> (a -> Dsl f b) -> Dsl f b
bind (Bind fa k) k2 = Bind fa (\x -> bind (k x) k2)
bind (Par fs k) k2 = Par fs (\l -> bind (k l) k2)
bind (Return a) k2 = k2 a

seq :: [Anim' obj] -> Anim' obj
seq [] = Return ()
seq (anim:r) = anim `bind` (\() -> seq r)

par :: [Anim' obj] -> Anim' obj
par l = Par l (\_ -> Return ())

test1 =
  Bind (Create 1 undefined) $ \[t0] ->
  seq
  [ par
    [ Bind (Base 1 undefined undefined) (Return)
    , Bind (Base 1 undefined undefined) (Return)
    ]
  , Bind (Base 1 undefined undefined) (Return)
  ]


stepDsl :: obj -> Float -> Dsl (Anim obj) a -> (obj, Dsl (Anim obj) a)
stepDsl obj t (Bind fa k) = let
  (newObj, eResult) = applyAnim obj t fa
  in case eResult of
    Left newAnim -> (newObj, Bind newAnim k)
    Right result -> (newObj, k result)
stepDsl obj t (Par fs k) = let
  (newObj, result) = applyAnims obj t fs
  in case returnValues result of
    Right l -> (newObj, k l)
    Left () -> (newObj, Par result k)
stepDsl obj t (Return a) = (obj, Return a)

returnValues :: [Dsl f a] -> Either () [a]
returnValues [] = Right []
returnValues ((Return a):r) = do
  l <- returnValues r
  return (a : l)
returnValues (_) = Left ()

applyAnims :: obj -> Float -> [Dsl (Anim obj) a] -> (obj, [Dsl (Anim obj) a])
applyAnims obj t [] = (obj, [])
applyAnims obj t (anim:r) = let
  (obj', animsAfterAnim) = stepDsl obj t anim
  (obj'', animsAfterRest) = applyAnims obj' t r
  in (obj'', animsAfterAnim : animsAfterRest)

applyAnim :: obj -> Float -> Anim obj a -> (obj, Either (Anim obj a) a)
applyAnim obj t (Base duration lens tween) = let
  -- update obj
  newObj = obj & lens %~ \current -> current + stepTween t duration tween current
  -- update anim
  newDuration = duration - t
  newAnim = if newDuration > 0
    then Left (Base (duration - t) lens tween)
    else Right ()
  in (newObj, newAnim)
applyAnim obj t (Set delay lens value) = let
  -- update obj, which is returned when delay runs out
  newObj = obj & lens .~ (value obj)
  -- update anim
  newDelay = delay - t
  in if newDelay > 0
      then (obj, Left (Set (delay - t) lens value))
      else (newObj, Right ())
applyAnim obj t (Create amount create) = let
  (newObj, indexList) = batchCreate amount obj create
  in (newObj, Right indexList)

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

ix' :: Int -> Lens' [a] a
ix' i = lens (!! i) (\s b -> take i s ++ b : drop (i+1) s)

