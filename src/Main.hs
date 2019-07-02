{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified World as World
import qualified Presentation as P

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Lens.Micro ((&))

main :: IO ()
main = play (InWindow "test" (800, 200) (10, 10)) black 60 P.initial P.draw P.handleInput P.update

-- main = play (InWindow "test" (800, 200) (10, 10)) black 60 World.initialWorld World.draw World.handleInput World.update
