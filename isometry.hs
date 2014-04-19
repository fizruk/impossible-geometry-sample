{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Control.Lens

import Data.Ord
import Data.Maybe
import Data.Graph
import Data.Monoid

import Data.Map (Map)
import qualified Data.Map as Map

import Graphics.Gloss.Interface.Pure.Game

import Debug.Trace

type Size = Float

type FigureId = Int

data Pos = Pos
  { _posX :: Float
  , _posY :: Float
  , _posZ :: Float }
  deriving (Eq, Show)
makeLenses ''Pos

instance Ord Pos where
  compare = comparing $ \(Pos x y z) -> (y, x + z)

instance Num Pos where
  Pos x y z + Pos x' y' z' = Pos (x + x') (y + y') (z + z')
  negate (Pos x y z) = Pos (-x) (-y) (-z)

(.*) :: Float -> Pos -> Pos
n .* Pos x y z = Pos (n * x) (n * y) (n * z)

type Face = [Pos]

type Figure = [Face]

faceClosestPoint :: Face -> Pos
faceClosestPoint = minimum

figureClosestPoint :: Figure -> Pos
figureClosestPoint = minimum . map faceClosestPoint

move :: Pos -> Figure -> Figure
move p = map $ map (+ p)

data WorldFigure = WorldFigure
  { _worldFigure   :: Figure
  , _worldFigureId :: FigureId }
  deriving (Show)
makeLenses ''WorldFigure

data World = World
  { _worldFigures :: [WorldFigure]
  , _worldRules   :: Map FigureId ([FigureId], [FigureId]) }
  deriving (Show)
makeLenses ''World

instance Monoid World where
  mempty = World mempty mempty
  World fs rs `mappend` World fs' rs' = World (fs <> fs') (rs <> rs')

type WorldBuilder = State ([FigureId], World)

freshFigureId :: WorldBuilder FigureId
freshFigureId = do
  x:xs <- use _1
  _1 .= xs
  return x

figure :: Figure -> WorldBuilder WorldFigure
figure x = do
  f <- WorldFigure x <$> freshFigureId
  _2.worldFigures %= (f:)
  return f

cube :: Size -> Pos -> WorldBuilder WorldFigure
cube s p = figure $ move p cube'
  where
    cube' = map (map (s .*)) $
      [ [Pos 0 0 0, Pos 0 0 1, Pos 0 1 1, Pos 0 1 0 ]
      , [Pos 0 0 0, Pos 0 1 0, Pos 1 1 0, Pos 1 0 0 ]
      , [Pos 0 0 0, Pos 1 0 0, Pos 1 0 1, Pos 0 0 1 ]
      , [Pos 1 1 1, Pos 1 0 1, Pos 1 0 0, Pos 1 1 0 ]
      , [Pos 1 1 1, Pos 0 1 1, Pos 0 0 1, Pos 1 0 1 ]
      , [Pos 1 1 1, Pos 1 1 0, Pos 0 1 0, Pos 0 1 1 ] ]

data Face2D = Face2D
  { getFace2D :: Path
  , faceLight :: Float }
  deriving (Show)

posTo2D :: Pos -> Point
posTo2D (Pos x y z) = (cos (pi / 6) * (x - z), y - sin (pi / 6) * (x + z) )

faceTo2D :: Face -> Maybe Face2D
faceTo2D ps@(p:q:r:_)
  | visible   = Just $ Face2D (map posTo2D ps) light
  | otherwise = Nothing
  where
    n = normal (p - q) (q - r)
    screenN = Pos 1 1 1
    lightN  = Pos 0.3 (- 1) 0
    light   = 0.5 * (1 + scalar n lightN)
    (p':q':r':_) = map posTo2D ps
    visible = orientation p' q' r' == GT

normal :: Pos -> Pos -> Pos
normal x = normalize . cross x

orientation :: Point -> Point -> Point -> Ordering
orientation (x, y) (u, v) (w, z) = ((x - u) * (v - z) - (u - w) * (y - v)) `compare` 0

scalar :: Pos -> Pos -> Float
scalar (Pos x y z) (Pos x' y' z') = x * x' + y * y' + z * z'

cross :: Pos -> Pos -> Pos
cross (Pos x y z) (Pos x' y' z') = Pos (y * z' - y' * z) (x * z' - x' * z) (x * y' - x' * y)

normalize :: Pos -> Pos
normalize p@(Pos x y z) = (1 / sqrt (x^2 + y^2 + z^2)) .* p

rotate2D :: Float -> Point -> Point -> Point
rotate2D a (ox, oy) (x, y) = (ox + x' * ca - y' * sa, oy + x' * sa + y' * ca)
  where
    (x', y') = (x - ox, y - oy)
    (ca, sa) = (cos a, sin a)

rotatePosX :: Float -> Pos -> Pos -> Pos
rotatePosX a (Pos _ oy oz) (Pos x y z) = Pos x y' z'
  where (y', z') = rotate2D a (oy, oz) (y, z)

rotatePosY :: Float -> Pos -> Pos -> Pos
rotatePosY a (Pos ox _ oz) (Pos x y z) = Pos x' y z'
  where (x', z') = rotate2D a (ox, oz) (x, z)

rotatePosZ :: Float -> Pos -> Pos -> Pos
rotatePosZ a (Pos ox oy _) (Pos x y z) = Pos x' y' z
  where (x', y') = rotate2D a (ox, oy) (x, y)

rotateX :: Float -> Pos -> Figure -> Figure
rotateX a p = map $ map (rotatePosX a p)

rotateY :: Float -> Pos -> Figure -> Figure
rotateY a p = map $ map (rotatePosY a p)

rotateZ :: Float -> Pos -> Figure -> Figure
rotateZ a p = map $ map (rotatePosZ a p)

drawFace2D :: Face2D -> Picture
drawFace2D (Face2D p l) = color (greyN l) $ polygon p

drawWorld :: World -> Picture
drawWorld w = pictures $ w ^.. worldFigures.traverse.worldFigure.to drawFigure

drawFigure :: Figure -> Picture
drawFigure = pictures . catMaybes . map (fmap drawFace2D . faceTo2D)

testWorld :: WorldBuilder ()
testWorld = do
  cube 1 (Pos 0 0 0)
  return ()

create :: WorldBuilder a -> World
create = snd . flip execState ([1..], mempty)

updateWorld :: World -> World
updateWorld = worldFigures.traverse.worldFigure %~ rotateY 0.1 (Pos 0 0 0)

main :: IO ()
main = play display' black 30 (create testWorld) (scale gameScale gameScale . drawWorld) (flip const) (const updateWorld)
  where
    display' = InWindow "Isometry" (640, 480) (200, 200)

gameScale :: Float
gameScale = 20

