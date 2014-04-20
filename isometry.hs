{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Applicative
import Control.Monad.State
import Control.Lens

import Data.Ord
import Data.Graph
import Data.List
import Data.Maybe
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
  compare = comparing $ \(Pos x y z) -> x + y + z

instance Num Pos where
  Pos x y z + Pos x' y' z' = Pos (x + x') (y + y') (z + z')
  negate (Pos x y z) = Pos (-x) (-y) (-z)

(.*) :: Float -> Pos -> Pos
n .* Pos x y z = Pos (n * x) (n * y) (n * z)

type Face = [Pos]

type Figure = [Face]

faceClosestPoint :: Face -> Pos
faceClosestPoint = maximum

move :: Pos -> Figure -> Figure
move p = map $ map (+ p)

data WorldFigure = WorldFigure
  { _worldFigure      :: Figure
  , _worldFigureColor :: Color
  , _worldFigureId    :: FigureId }
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

figure :: Color -> Figure -> WorldBuilder WorldFigure
figure c x = do
  f <- WorldFigure x c <$> freshFigureId
  _2.worldFigures %= (f:)
  return f

cube :: Size -> Color -> Pos -> WorldBuilder WorldFigure
cube s c p = figure c $ move p cube'
  where
    cube' = map (map (s .*)) $
      [ [Pos 0 0 0, Pos 0 0 1, Pos 0 1 1, Pos 0 1 0 ]
      , [Pos 0 0 0, Pos 0 1 0, Pos 1 1 0, Pos 1 0 0 ]
      , [Pos 0 0 0, Pos 1 0 0, Pos 1 0 1, Pos 0 0 1 ]
      , [Pos 1 1 1, Pos 1 0 1, Pos 1 0 0, Pos 1 1 0 ]
      , [Pos 1 1 1, Pos 0 1 1, Pos 0 0 1, Pos 1 0 1 ]
      , [Pos 1 1 1, Pos 1 1 0, Pos 0 1 0, Pos 0 1 1 ]
      ]

data Face2D = Face2D
  { _getFace2D :: Path
  , _faceLight :: Float }
  deriving (Show)
makeLenses ''Face2D

posTo2D :: Pos -> Point
posTo2D (Pos x y z) = (cos (pi / 6) * (x - z), y - sin (pi / 6) * (x + z) )

faceTo2D :: Face -> Maybe Face2D
faceTo2D ps@(p:q:r:_)
  | visible   = Just $ Face2D (map posTo2D ps) light
  | otherwise = Nothing
  where
    n = normal (p - q) (q - r)
    screenN = Pos 1 1 1
    lightN  = Pos 1 (- 3) (- 1)
    light   = 0.5 * (1 + scalar n lightN)
    (p':q':r':_) = map posTo2D ps
    visible = orientation p' q' r' == GT

normal :: Pos -> Pos -> Pos
normal x = normalize . cross x

orientation :: Point -> Point -> Point -> Ordering
orientation p q r = cross2D (p .- q) (q .- r) `compare` 0

scalar :: Pos -> Pos -> Float
scalar (Pos x y z) (Pos x' y' z') = x * x' + y * y' + z * z'

cross2D :: Point -> Point -> Float
cross2D (x, y) (x', y') = x * y' - x' * y

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

overlapEdges :: [Point] -> [Point] -> Bool
overlapEdges xs ys = or $ zipWith f ex ey
  where
    ex = zip xs $ tail (cycle xs)
    ey = zip ys $ tail (cycle ys)
    f (p, q) (p', q')
      =  orientation p  q  p' /= orientation p  q  q'
      && orientation p' q' p  /= orientation p' q' q

overlap :: [Point] -> [Point] -> Bool
overlap = any . inside
  where
    inside :: [Point] -> Point -> Bool
    inside xs y = all (f y) $ zip xs (tail $ cycle xs)

    f :: Point -> (Point, Point) -> Bool
    f p (a, b) = 0 < cross2D (a .- p) (b .- p)

drawWorld :: World -> Picture
drawWorld w = traceShow g $ pictures $ map (uncurry drawFace2D) faces''
  where
    figures = w ^. worldFigures
    rules = w ^. worldRules
    faces = [ (face, face2D, figId, figCol)
            | fig <- figures
            , face <- fig ^. worldFigure
            , face2D <- face ^.. to faceTo2D.traverse
            , let figId = fig ^. worldFigureId
            , let figCol = fig ^. worldFigureColor ]
    faces' = zip [1..] faces
    edges = [ ((figCol, face2D), k, ks)
            | (k, (face, face2D, figId, figCol)) <- faces'
            , let (behinds, aheads) = Map.findWithDefault mempty figId rules
            , let ks = [ k'
                       | (k', (face', face2D', figId', _)) <- faces'
                       , k /= k'
                       , overlap (face2D ^. getFace2D) (face2D' ^. getFace2D)
                       || overlapEdges (face2D ^. getFace2D) (face2D' ^. getFace2D)
                       , figId' `elem` aheads || figId' `notElem` behinds &&
                         faceClosestPoint face' > faceClosestPoint face
                       ] ]
    (g, fromV, _) = graphFromEdges edges
    faces'' = map (view _1 . fromV) $ topSort g

drawFace2D :: Color -> Face2D -> Picture
drawFace2D c (Face2D p l) = color (c `addColors` greyN l) $ polygon p

cubes :: Size -> Color -> [Pos] -> WorldBuilder [WorldFigure]
cubes s c = mapM $ cube s c

addBehinds :: [FigureId] -> ([FigureId], [FigureId]) -> ([FigureId], [FigureId])
addBehinds xs (bs, as) = (bs <> xs, as \\ xs)

addAheads :: [FigureId] -> ([FigureId], [FigureId]) -> ([FigureId], [FigureId])
addAheads xs (bs, as) = (bs \\ xs, as <> xs)

behind :: [WorldFigure] -> [WorldFigure] -> WorldBuilder ()
behind xs ys = do
  mapM_ (\x -> _2.worldRules.at x %= Just . addAheads  ys' . fromMaybe mempty) xs'
  mapM_ (\y -> _2.worldRules.at y %= Just . addBehinds xs' . fromMaybe mempty) ys'
  where
    xs' = map (view worldFigureId) xs
    ys' = map (view worldFigureId) ys

testWorld :: WorldBuilder ()
testWorld = do
  lx <- cubes 0.9 (dark red)   [ Pos n 4 0     | n <- [0..3] ]
  ly <- cubes 0.9 (dark green) [ Pos 0 n 0     | n <- [0..3] ]
  lz <- cubes 0.9 (dark blue)  [ Pos 0 0 (- n) | n <- [1..4] ]

  lx `behind` lz

create :: WorldBuilder a -> World
create = join traceShow . snd . flip execState ([1..], mempty)

updateWorld :: World -> World
updateWorld = worldFigures.traverse.worldFigure %~ rotateY (- 0.1) (Pos 0 0 0)

main :: IO ()
main = play display' black 10 (create testWorld) (scale gameScale gameScale . drawWorld) (flip const) (const updateWorld)
  where
    display' = InWindow "Isometry" (640, 480) (200, 200)

gameScale :: Float
gameScale = 20

(.-) :: Vector -> Vector -> Vector
(x, y) .- (x', y') = (x - x', y - y')

