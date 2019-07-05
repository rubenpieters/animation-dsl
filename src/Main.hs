{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified World as World
import qualified Presentation as P

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro ((&))

main :: IO ()
main = World.main
{-main = do
  (Just kulLogo) <- loadJuicyPNG ("assets/kul.png")
  let cache = Map.fromList [("kulLogo", kulLogo)]
  play (InWindow "test" (1024, 768) (10, 10)) black 60 (P.initial cache) P.draw P.handleInput P.update
-}
