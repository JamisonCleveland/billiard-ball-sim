{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad (replicateM)
import qualified Graphics.Gloss.Data.Point.Arithmetic as Vec
import Graphics.Gloss.Interface.Pure.Simulate
import System.Random (randomRIO)

main :: IO ()
main = do
  init_ <- replicateM numberOfBalls do
    x <- randomRIO (- ballPositionBound, ballPositionBound)
    y <- randomRIO (- ballPositionBound, ballPositionBound)
    theta <- randomRIO (0, 2 * pi)
    r <- randomRIO (0, 1)
    g <- randomRIO (0, 1)
    b <- randomRIO (0, 1)
    alpha <- randomRIO (0.25, 1)
    pure
      Ball
        { pos = (x, y),
          dir = (cos theta, sin theta),
          color_ = makeColor r g b alpha
        }
  simulate windowCfg backgroundColor fps init_ draw update
  where
    numberOfBalls = 50

    windowCfg = InWindow title size pos
      where
        title = "asdf"
        size = (500, 500)
        pos = (100, 100)
    fps = 60
    backgroundColor = white

type State = [Ball]

data Ball = Ball {pos :: Vector, dir :: Vector, color_ :: Color}

boxLength :: Float
boxLength = 500

ballPositionBound :: Float
ballPositionBound = boxLength / 2 - ballRadius

ballRadius :: Float
ballRadius = 20

ballSpeed :: Float
ballSpeed = 200

draw :: State -> Picture
draw state =
  pictures
    [ rectangleWire boxLength boxLength,
      pictures
        [ translate x y $ color color_ $ circleSolid ballRadius
          | Ball {pos = (x, y), color_} <- state
        ]
    ]

update :: ViewPort -> Float -> State -> State
update _ delta = map (resolveCollision . moveBall)
  where
    moveBall :: Ball -> Ball
    moveBall ball@Ball {pos, dir} =
      ball {pos = pos'}
      where
        vel = delta * ballSpeed Vec.* dir
        pos' = pos Vec.+ vel
    
    resolveCollision :: Ball -> Ball
    resolveCollision ball@Ball {pos = (x, y), dir = (dirX, dirY)} =
      let (x', dirX') = axisCollision x dirX
          (y', dirY') = axisCollision y dirY
       in ball {pos = (x', y'), dir = (dirX', dirY')}
      where
        axisCollision :: Float -> Float -> (Float, Float)
        axisCollision p d
          | p < - ballPositionBound = (- ballPositionBound, - d)
          | p > ballPositionBound = (ballPositionBound, - d)
          | otherwise = (p, d)