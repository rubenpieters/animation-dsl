module Font where

import Data.Function ((&))

import Graphics.Gloss

{-

[ []
, []
]

-}

data Pixel = X | O
  deriving (Show, Eq, Ord)

type Layout = [[Pixel]]

space =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  ]

lowerA =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,X,X,X,X,O,O,O]
  , [O,O,O,O,O,O,O,X,X,O,O]
  , [O,O,O,X,X,X,X,X,X,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,X,O,X,X,X,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]

lowerB =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  , [O,O,X,X,O,X,X,X,O,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,X,O,X,X,X,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]

lowerD =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,X,X,O,O]
  , [O,O,O,O,O,O,O,X,X,O,O]
  , [O,O,O,X,X,X,O,X,X,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,X,O,X,X,X,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]

lowerE =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]

lowerI =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  ]

lowerL =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,X,X,O,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,O,X,X,O,O,O,O]
  ]

lowerM =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,X,X,O,X,X,O,X,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,O,X,X,O,X,O,O]
  , [O,O,X,X,O,X,X,O,X,O,O]
  , [O,O,X,X,O,X,X,O,X,O,O]
  , [O,O,X,X,O,X,X,O,X,O,O]
  ]

lowerN =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,X,X,O,X,X,X,O,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,O,O,O]
  ]

lowerO =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  , [O,O,X,X,X,X,X,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]

lowerP =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,X,X,O,X,X,X,O,O,O]
  , [O,O,X,X,X,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,X,X,X,X,O,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  ]

lowerR =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,X,X,O,X,X,O,O,O]
  , [O,O,O,X,X,X,X,O,O,O,O]
  , [O,O,O,X,X,O,O,O,O,O,O]
  , [O,O,O,X,X,O,O,O,O,O,O]
  , [O,O,O,X,X,O,O,O,O,O,O]
  , [O,O,O,X,X,O,O,O,O,O,O]
  ]

lowerS =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  , [O,O,X,X,X,X,X,O,O,O,O]
  , [O,O,X,X,O,O,O,O,O,O,O]
  , [O,O,O,X,X,X,X,O,O,O,O]
  , [O,O,O,O,O,O,X,X,O,O,O]
  , [O,O,X,X,X,X,X,O,O,O,O]
  ]

lowerT =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,X,X,O,O,O]
  , [O,O,O,O,X,X,X,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,X,X,O,O,O,O,O]
  , [O,O,O,O,O,X,X,X,O,O,O]
  ]

lowerU =
  [ [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,O,O,O,O,O,O,O,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,O,O,O,X,X,O,O]
  , [O,O,X,X,X,O,X,X,X,O,O]
  , [O,O,O,X,X,X,X,X,O,O,O]
  ]


neg :: [Float]
neg = let
  negInt' x = x : negInt' (x - 1)
  in negInt' 0

addCoordinates :: Layout -> [(Pixel, Float, Float)]
addCoordinates l = let
  withY ps = zip ps neg
  withX ps = zip ps [0..]
  in foldMap (\(l, y) -> map (\(p, x) -> (p, x, y)) (withX l)) (withY l)


pixel :: Picture
pixel = Polygon [(0,0),(1,0),(1,1),(0,1),(0,0)]

pixelListToPicture :: [(Pixel, Float, Float)] -> Picture
pixelListToPicture l = let
  f (X, x, y) = pixel & Translate x y
  f (O, x, y) = Blank
  in Pictures (map f l)

layoutToPic :: Layout -> Picture
layoutToPic = pixelListToPicture . addCoordinates

charToLayout :: Char -> Layout
charToLayout 'a' = lowerA
charToLayout 'b' = lowerB
charToLayout 'd' = lowerD
charToLayout 'e' = lowerE
charToLayout 'i' = lowerI
charToLayout 'l' = lowerL
charToLayout 'm' = lowerM
charToLayout 'n' = lowerN
charToLayout 'o' = lowerO
charToLayout 'p' = lowerP
charToLayout 'r' = lowerR
charToLayout 's' = lowerS
charToLayout 't' = lowerT
charToLayout 'u' = lowerU
charToLayout _ = undefined

stringToPic :: String -> [Picture]
stringToPic s = map (layoutToPic . charToLayout) s
