module Space where

import Data.IORef
import Data.List
import Control.Monad

import Graphics.Rendering.Cairo
-- import Graphics.Rendering.Cairo.Matrix
-- import Graphics.UI.Gtk


data Space = Space {
           sRunning :: Bool,
           sSpheres :: [Sphere]
     } deriving (Show)


gravity :: Double
gravity = 0.07

-- Alle Daten sind relativ, d.h. normiert auf eins !!
data Sphere = Sphere {
     colorRGB  :: (Double, Double, Double),
     xCoord    :: Double,
     yCoord    :: Double,
     -- der Radius ist relativ zu der Breite des Fensters !!!
     radius    :: Double,
     xSpeed    :: Double,
     ySpeed    :: Double,
     mass      :: Double
}  deriving (Show)


initSpace :: IO (IORef Space)
initSpace = newIORef Space {sRunning = True, sSpheres = [s1, s2, s3, s4]}

s1 = Sphere {
     colorRGB = (1, 0, 0),
     xCoord = 0.5,
     yCoord = 0.5,
     radius = 0.05,
     xSpeed = -0.0007,
     ySpeed = -0.0004,
     mass   = 0.05^3
}

s2 = Sphere {
     colorRGB = (0, 1, 0),
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
     ySpeed = 0.007,
     mass   = 0.01^3
}

drawSpace :: IORef Space -> Double -> Double -> IO (Render ())
drawSpace refSpace width height = do -- IO Monad
    space <- readIORef refSpace
    let balls = sSpheres space
    let render = flip map balls $ \s -> do -- Render Monad
          let (r, g, b) = colorRGB s
          setSourceRGB r g b
          arc (xCoord s) (yCoord s) (radius s) 0 (2*pi)
          fill

    when (sRunning space == True) $ modifyIORef refSpace refreshSpace

    ---- DEBUG:
    -- dSpace <- readIORef refSpace
    -- putStrLn $ show dSpace

    return $ foldl' (>>) startDraw render >> endDraw

    where startDraw = save >> scale width height
          endDraw   = restore >> stroke

refreshSpace :: Space -> Space
refreshSpace space =
    space {sSpheres = (newSpeed . newCoord) (sSpheres space)}

newCoord :: [Sphere]-> [Sphere]
newCoord ss = flip map (ss) $ \s ->
    let   x  = xCoord s
          y  = yCoord s
          vx = xSpeed s
          vy = ySpeed s
    in s {xCoord = x + vx, yCoord = y + vy}

-- TODO: diese Funktion nochmal implemtieren
newSpeed :: [Sphere] -> [Sphere]
newSpeed []      = []
newSpeed xs = flip map xs $ \x ->
         let (dVx, dVy) = foldl' calc (0,0) xs
             calc (ax, ay) y =
                 let d2 = distance2 x y
                     d  = sqrt d2
                     dax = gravity * (mass y) / d2 / d * (xCoord y - xCoord x)
                     day = gravity * (mass y) / d2 / d * (yCoord y - yCoord x)
                 in
                    case d2 of
                      0 -> (ax, ay)
                      _ -> (ax + dax, ay + day)
         in x { xSpeed = xSpeed x + dVx, ySpeed = ySpeed x + dVy}



-- | quadratische Entfernung zwischen zwei `Sphere`
distance2 :: Sphere -> Sphere -> Double
distance2 x y = (xCoord x - xCoord y)^2 + (yCoord x - yCoord y)^2

distance :: Sphere -> Sphere -> Double
distance x y = sqrt $ (xCoord x - xCoord y)^2 + (yCoord x - yCoord y)^2
