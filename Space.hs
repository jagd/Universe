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
     }

-- Alle Daten sind relativ, d.h. normiert auf eins !!
data Sphere = Sphere {
     xCoord :: Double,
     yCoord :: Double,
     -- der Radius ist relativ zu der Breite des Fensters !!!
     radius :: Double,
     xSpeed :: Double,
     ySpeed :: Double
}


initSpace :: IO (IORef Space)
initSpace = newIORef Space {sRunning = True, sSpheres = [s1, s2]}

s1 = Sphere {
     xCoord = 0.3,
     yCoord = 0.5,
     radius = 0.05,
     xSpeed = 0.001,
     ySpeed = 0.004
}

s2 = Sphere {
     xCoord = 0.4,
     yCoord = 0.7,
     radius = 0.03,
     xSpeed = 0.002,
     ySpeed = -0.003
}

drawSpace :: IORef Space -> Double -> Double -> IO (Render ())
drawSpace refSpace width height = do -- IO Monad
    space <- readIORef refSpace
    let balls = sSpheres space
    let render = flip map balls $ \s -> do -- Render Monad

          setSourceRGB 1 0 0
          setLineWidth 5
          arc (xCoord s * width) (yCoord s * height) (radius s * width) 0 (2*pi)
          fill

          stroke

    when (sRunning space == True) $ modifyIORef refSpace refreshSpace 

    return $ foldl' (>>) (return ()) render
          
refreshSpace :: Space -> Space
refreshSpace space =
      space {sSpheres = newSpheres space}
      where newSpheres space = flip map (sSpheres space) $ \s -> 
                let   x  = xCoord s
                      y  = yCoord s
                      vx = xSpeed s
                      vy = ySpeed s
                in s {xCoord = x + vx, yCoord = y + vy}
