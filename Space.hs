module Space where

import Data.List
import Control.Monad

import Graphics.Rendering.Cairo

import Sphere

timePerTick :: Int
timePerTick = 10



data SpaceStatus = SSRunning
                 | SSCrashed
                 | SSReady
                 | SSDone
                 | SSInfo Double String SpaceStatus
                   --   FontSize Message NextStatus
     deriving (Show, Eq)


data Space = Space {
           sStatus     :: SpaceStatus,
           gravity     :: Double,
           sSpheres    :: [Sphere],
           sController :: Sphere,
           sTime       :: Int
     } deriving (Show)


-- einige merkwürdige Spaces

infinitInfoStatus fs msg = SSInfo fs msg $ infinitInfoStatus fs msg

errorSpace fs msg = Space {
       sStatus = infinitInfoStatus fs msg,
       sController = Sphere {
                         colorRGB = (1, 1, 1),
                         xCoord = -2,
                         yCoord = -2,
                         radius = 0.03,
                         xSpeed = 0,
                         ySpeed = 0,
                         mass   = 0
                      },
       gravity = 0.000,
       sSpheres = [],
       sTime = 1
}


-- Space Manipulation

updateController :: Double -> Double -> Space -> Space
updateController x y s =
        let c = sController s
            c' = c {xCoord = x, yCoord = y}
        in s {sController = c'}


spaceNext :: Space -> Space
spaceNext space
        | sStatus space == SSRunning =
             let g = gravity space
                 timeLeft = sTime space - timePerTick
                 s' = space {
                       sSpheres = (newSpeeds1 g $ sController space)
                                   . newSpeeds g
                                   . newCoords
                                   $ (sSpheres space),
                       sTime = if timeLeft > 0 then timeLeft else 0
                      }
                 spheres = sSpheres s'
                 c = sController s'
                 s'' = if sTime space <= 0
                         then s' {sStatus = SSDone}
                         else s'
             in if outBound spheres || collision (c:spheres)
               then s'' {sStatus = SSCrashed}
               else s''
        | otherwise = space

drawSpace :: Space -> Double -> Double -> Render ()
drawSpace space width height =
        let balls = sSpheres space
            c = sController space
            render = flip map (c:balls) drawSphere
        in foldl' (>>) startDraw
                             render
                             >> drawTime
                             >> drawStatus
                             >> endDraw
        where startDraw = do save >> scale width height
              endDraw   = stroke >> restore
              drawTime  = do
                        setSourceRGB 0 1 0
                        setFontSize 0.04
                        moveTo 0.01 0.05
                        let milliSec = sTime space
                        let (sec, csec) = (milliSec `div` 100) `divMod` 10
                        showText $ "Left Time  " ++ (show sec) ++ "." ++ (show $ csec)
              drawStatus =
                  case sStatus space of
                       SSCrashed -> do
                           setSourceRGBA 0 0 0 0.5
                           rectangle 0 0 1 1
                           fill
                           setSourceRGB 1 0 0
                           setFontSize 0.07
                           moveTo 0.1 0.5
                           showText "The Universe Crashed !"
                           --
                           setSourceRGB 1 1 0
                           setFontSize 0.04
                           moveTo 0.2 0.6
                           showText "click anywhere to restart"
                       SSReady -> do
                           setSourceRGBA 0 0 0 0.5
                           rectangle 0 0 1 1
                           fill
                           setSourceRGB 1 0 0
                           setFontSize 0.09
                           moveTo 0.3 0.4
                           showText "Ready ?"
                           --
                           setSourceRGB 1 1 0
                           setFontSize 0.04
                           moveTo 0.3 0.6
                           showText "click anywhere to start"
                       SSDone -> do
                           setSourceRGBA 0 0 0 0.5
                           rectangle 0 0 1 1
                           fill
                           setSourceRGB 0 1 0
                           setFontSize 0.09
                           moveTo 0.3 0.4
                           showText "Level Done"
                           --
                           setSourceRGB 1 1 0
                           setFontSize 0.04
                           moveTo 0.35 0.6
                           showText "click to continue"
                       SSInfo fs msg _ -> do
                           setSourceRGBA 0 0 0 0.5
                           rectangle 0 0 1 1
                           fill
                           setSourceRGB 0 1 0
                           setFontSize fs
                           moveTo 0.3 0.4
                           showText msg
                       _ -> return ()
