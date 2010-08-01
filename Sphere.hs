module Sphere (
       Sphere(..),
       drawSphere,      
       newSpeeds1,
       newSpeeds,
       newCoords,
       outBound,
       collision
       ) where

import Data.List

import Graphics.Rendering.Cairo


-- | Alle Daten sind relative Werte, d.h. normiert auf eins !!
data Sphere = Sphere {
     colorRGB  :: (Double, Double, Double),
     xCoord    :: Double,
     yCoord    :: Double,
     radius    :: Double,
     xSpeed    :: Double,
     ySpeed    :: Double,
     mass      :: Double
}  deriving (Show)




-- | quadratische Entfernung zwischen zwei `Sphere`
distance2 :: Sphere -> Sphere -> Double
distance2 x y = (xCoord x - xCoord y)^2 + (yCoord x - yCoord y)^2

-- | Entfernung zwischen zwei `Sphere`
distance :: Sphere -> Sphere -> Double
distance x y = sqrt $ (xCoord x - xCoord y)^2 + (yCoord x - yCoord y)^2

-- | Die Kraftwirkung (Beschleunigungswirkung) des eine Kugel
-- (z.B. die Mause -- der Regler) zu den anderen 
newSpeeds1 :: Double -> Sphere -> [Sphere] -> [Sphere] 
newSpeeds1 g c xs = flip map xs $ \x ->  -- g : gravity
         let (dVx, dVy) = calc c
             calc y =
                 let d2 = distance2 x y
                     d  = sqrt d2
                     dax = g * (mass y) / d2 / d * (xCoord y - xCoord x)
                     day = g * (mass y) / d2 / d * (yCoord y - yCoord x)
                 in case d2 of
                      0 -> (0, 0)
                      _ -> (dax, day)
         in x { xSpeed = xSpeed x + dVx, ySpeed = ySpeed x + dVy}


-- TODO: diese Funktion muss nochmal implemtiert werden
-- | die Kraftwirkung (Beschleunigungswirkung) jeder einzige Kugel
--   zun den anderen in einer Sphere-Gruppe
newSpeeds :: Double -> [Sphere] -> [Sphere]
newSpeeds _ [] = []
newSpeeds g xs = flip map xs $ \x ->
         let (dVx, dVy) = foldl' calc (0,0) xs
             calc (ax, ay) y =
                 let d2 = distance2 x y
                     d  = sqrt d2
                     dax = g * (mass y) / d2 / d * (xCoord y - xCoord x)
                     day = g * (mass y) / d2 / d * (yCoord y - yCoord x)
                 in case d2 of
                      0 -> (ax, ay)
                      _ -> (ax + dax, ay + day)
         in x { xSpeed = xSpeed x + dVx, ySpeed = ySpeed x + dVy}


-- | Die neue Lage jeder Kugel aus einer Gruppe neu zu rechnen
newCoords :: [Sphere]-> [Sphere]
newCoords ss = flip map (ss) $ \s ->
    let   x  = xCoord s
          y  = yCoord s
          vx = xSpeed s
          vy = ySpeed s
    in s {xCoord = x + vx, yCoord = y + vy}

-- | Einzele Kugel zu zeichnen
drawSphere :: Sphere -> Render()
drawSphere s = do -- Render Monad
        let (r, g, b) = colorRGB s
        -- Linear Farbeveränderung
        withLinearPattern (xCoord s) (yCoord s - radius s)
                          (xCoord s) (yCoord s + radius s)
                          $ \pattern -> do
                patternAddColorStopRGBA pattern 0 r g b 1
                patternAddColorStopRGBA pattern 1 r g b 0.25
                setSource pattern
        arc (xCoord s) (yCoord s) (radius s) 0 (2*pi)
        fill
        -- Kugel Farbeveränderung
        withRadialPattern (xCoord s) (yCoord s) 0 (xCoord s) (yCoord s) (radius s)
            $ \pattern -> do
                patternAddColorStopRGBA pattern 0 r g b 1
                patternAddColorStopRGBA pattern 1 r g b 0
                setSource pattern
        arc (xCoord s) (yCoord s) (radius s) 0 (2*pi)
        fill


outBound :: [Sphere] -> Bool
outBound xs = flip any xs $ \s ->
    let (x,y) = (xCoord s, yCoord s)
    in f x y
    where f x y
            | x <= 0 = True
            | x >= 1 = True
            | y <= 0 = True
            | y >= 1 = True
            | otherwise = False


collision :: [Sphere] -> Bool
collision [] = False
collision (x:xs)
    | collision xs == True = True
    | otherwise = flip any xs $ \s -> collision' x s
    where collision' a b =
               if distance a b > radius a + radius b
                 then False
                 else True
