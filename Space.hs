module Space where

import Data.IORef
import Data.List
import Control.Monad

import Graphics.Rendering.Cairo

import Sphere

timePerTick :: Int
timePerTick = 10

data Space = Space {
           gravity     :: Double,
           sRunning    :: Bool,
           sSpheres    :: [Sphere],
           sController :: Sphere,
           sTime       :: Int
     } deriving (Show)


initSpace :: IO (IORef Space)
initSpace = newIORef Space {
                     sRunning = True,
                     sController = mouse,
                     gravity = 0.007,
                     -- sSpheres = [s1, s2, s3, s4, s5],
                     sSpheres = [s4, s5],
                     sTime = 0
                     }


updateController :: Double -> Double -> IORef Space -> IO ()
updateController x y space = modifyIORef space $ \s ->
                 let c = sController s
                     c' = c {xCoord = x, yCoord = y}
                 in s {sController = c'}


spaceNext :: IORef Space -> IO ()
spaceNext refSpace = do
    bRunning <- sRunning `liftM` readIORef refSpace
    when ( bRunning == True ) $ modifyIORef refSpace $ \space ->
        let g = gravity space
        in space {
            sSpheres = (newSpeeds1 g $ sController space)
                          . newSpeeds g
                          . newCoords
                          $ (sSpheres space),
            sTime = sTime space + timePerTick
           }


drawSpace :: Space -> Double -> Double -> IO (Render ())
drawSpace space width height = do -- IO Monad
        let balls = sSpheres space
        let c = sController space
        let render = flip map (c:balls) drawSphere

        return $ foldl' (>>) startDraw render >> endDraw
        where startDraw = do
                        save
                        scale width height
              endDraw   = do
                        stroke
                        setSourceRGB 1 1 0
                        setFontSize 0.04
                        moveTo 0.01 0.05
                        let milliSec = sTime space
                        let (sec, csec) = (milliSec `div` 100) `divMod` 10
                        showText $ "Time  " ++ (show sec) ++ "." ++ (show $ csec)
                        restore


-- Spheres für Debugzweck

mouse = Sphere {
     colorRGB = (1, 1, 1),
     xCoord = 0.5,
     yCoord = 0.7,
     radius = 0.03,
     xSpeed = 0.004,
     ySpeed = -0.00,
     mass   = 0.05^3 -- könnte ein bisschen größer sein
}


s1 = Sphere {
     colorRGB = (1, 0, 0),
     xCoord = 0.5,
     yCoord = 0.5,
     radius = 0.05,
     xSpeed = -0.0009,
     ySpeed = 0.000,
     mass   = 0.05^3
}

s2 = Sphere {
     colorRGB = (1, 1, 0.5),
     xCoord = 0.5,
     yCoord = 0.7,
     radius = 0.03,
     xSpeed = 0.004,
     ySpeed = -0.00,
     mass   = 0.03^3
}

s3 = Sphere {
     colorRGB = (0, 0, 1),
     xCoord = 0.5,
     yCoord = 0.8,
     radius = 0.005,
     xSpeed = -0.006,
     ySpeed = 0.001,
     mass   = 0.005^3
}

s4 = Sphere {
     colorRGB = (1, 0, 1),
     xCoord = 0.7,
     yCoord = 0.5,
     radius = 0.01,
     xSpeed = -0.00,
     ySpeed = 0.005,
     mass   = 0.01^3
}

s5 = Sphere {
     colorRGB = (62/255, 224/255, 205/255),
     xCoord = 0.7,
     yCoord = 0.7,
     radius = 0.01,
     xSpeed = 0.005,
     ySpeed = -0.001,
     mass   = 0.01^3
}
