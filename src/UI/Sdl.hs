{-# LANGUAGE OverloadedStrings #-}

module UI.Sdl (main) where

import Control.Concurrent
import Control.Monad (unless)
import SDL
import SDL.Vect
import System.Random (randomRIO)

data Ball = Ball
  { ballPosition :: V2 Float
  , ballVelocity :: V2 Float
  , ballSize     :: Float
  }

updateBall :: Ball -> Float -> Ball
updateBall ball deltaTime =
  let V2 x y = ballPosition ball
      V2 vx vy = ballVelocity ball
      size = ballSize ball
      newPos = ballPosition ball + fmap (* deltaTime) (ballVelocity ball)
      newVel | x + size > 800 || x - size < 0 = V2 (- vx) vy
             | y + size > 600 || y - size < 0 = V2 vx (- vy)
             | otherwise = ballVelocity ball
  in ball { ballPosition = newPos, ballVelocity = newVel }

main :: IO ()
main = do
  initializeAll
  window <- createWindow "Haskell SDL Bouncing Ball" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  ball <- createRandomBall
  appLoop renderer ball

  destroyRenderer renderer
  destroyWindow window
  quit

createRandomBall :: IO Ball
createRandomBall = do
  x <- randomRIO (100, 700)
  y <- randomRIO (100, 500)
  vx <- randomRIO (-200, 200)
  vy <- randomRIO (-200, 200)
  size <- randomRIO (10, 30)
  return $ Ball (V2 x y) (V2 vx vy) size

appLoop :: Renderer -> Ball -> IO ()
appLoop renderer ball = do
  events <- pollEvents
  let quitEvent e = case eventPayload e of
        QuitEvent -> True
        _         -> False
      q = any quitEvent events

  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- Draw ball
  rendererDrawColor renderer $= V4 255 255 255 255
  let V2 x y = ballPosition ball
      size = ballSize ball
  fillRect renderer $ Just $ Rectangle (P $ V2 (round $ x - size) (round $ y - size)) (V2 (round $ size * 2) (round $ size * 2))

  present renderer

  -- Update ball
  let deltaTime = 1 / 60
  let newBall = updateBall ball deltaTime

  threadDelay 16000 -- Approximately 60 FPS (1000 ms / 60 = 16.67 ms per frame)
  unless q $ appLoop renderer newBall
