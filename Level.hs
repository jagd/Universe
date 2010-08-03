module Level where

import System.IO.Error

import Space
import Sphere

type Level = [Space]

savedGamePath = ".UniverseSave"

-- | load saved game information from disk
loadLevelNum :: IO Int
loadLevelNum = do
               r <- try (readFile savedGamePath)
               case r of
                 Left _  -> do
                            putStrLn $ "Could not find any saved game in "
                                       ++ savedGamePath
                            return 1
                 Right s -> return (read s)

saveLevelNum :: Int -> IO ()
saveLevelNum n = writeFile savedGamePath $ show n

loadLevelData :: IO Level
loadLevelData = return buildinMaps

levelToSpace :: Level -> Int -> Space
levelToSpace l n
        | n <= 0 || n > length l =
                      errorSpace 0.05 $  "Could not load level " ++ show n
        | otherwise    = head $ drop (n-1) l





buildinMaps = [l1, l2, l3]

defaultCursor = Sphere {
     colorRGB = (1, 1, 1),
     xCoord = -2,
     yCoord = -2,
     radius = 0.03,
     xSpeed = 0,
     ySpeed = 0,
     mass   = 0.05^3 -- könnte ein bisschen größer sein
}

----------------------------------------
--   Level 1
----------------------------------------

l1 = Space {
       sStatus = SSInfo 0.09 "Level 1" SSReady,
       sController = defaultCursor,
       gravity = 0.007,
       sSpheres = [l1s1],
       sTime = 10000
     }

l1s1 = Sphere {
     colorRGB = (1, 0, 1),
     xCoord = 0.7,
     yCoord = 0.5,
     radius = 0.01,
     xSpeed = -0.00,
     ySpeed = 0.001,
     mass   = 0.01^3
}

----------------------------------------
--   Level 2
----------------------------------------

l2 = Space {
       sStatus = SSInfo 0.09 "Level 2" SSReady,
       sController = defaultCursor,
       gravity = 0.007,
       sSpheres = [l2s1],
       sTime = 10000
     }

l2s1 = Sphere {
     colorRGB = (1, 0, 0),
     xCoord = 0.5,
     yCoord = 0.5,
     radius = 0.05,
     xSpeed = -0.0009,
     ySpeed = 0.000,
     mass   = 0.05^3
}

----------------------------------------
--   Level 2
----------------------------------------

l3 = Space {
       sStatus = SSInfo 0.09 "Level 3" SSReady,
       sController = l3cursor,
       gravity = 0.3,
       sSpheres = [l3s1, l3s2],
       sTime = 10000
     }

l3cursor = Sphere {
     colorRGB = (1, 1, 1),
     xCoord = 10,
     yCoord = 10,
     radius = 0.02,
     xSpeed = 0.004,
     ySpeed = -0.00,
     mass   = 0.02^3 -- könnte ein bisschen größer sein
}

l3s1 = Sphere {
     colorRGB = (1, 0, 1),
     xCoord = 0.5,
     yCoord = 0.4,
     radius = 0.015,
     xSpeed = -0.002,
     ySpeed = 0.00,
     mass   = 0.015^3
}

l3s2 = Sphere {
     colorRGB = (62/255, 224/255, 205/255),
     xCoord = 0.5,
     yCoord = 0.6,
     radius = 0.015,
     xSpeed = 0.002,
     ySpeed = -0.000,
     mass   = 0.015^3
}
