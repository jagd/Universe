module Widget where

import Control.Monad (liftM)
import Data.IORef

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Space
import Level

main :: IO ()
main= do
     initGUI
     window <- windowNew
     canvas <- drawingAreaNew
     set window [windowTitle := "The Space",
                 windowDefaultWidth := 550,
                 windowDefaultHeight := 550,
                 containerBorderWidth := 5,
                 containerChild := canvas
                 ]
     widgetModifyBg canvas StateNormal (Color 0 0 0)
     widgetShowAll window

     ---------------
     -- Data Init --
     ---------------

     levels <- loadLevelData
     n      <- loadLevelNum 
     refLevelNum <- newIORef n
     refSpace <- newIORef $ levelToSpace levels n

     -------------
     -- Events: --
     -------------
     onExpose canvas (\x -> doExpose canvas refSpace >> return True)
     onButtonPress canvas (\x -> doClick canvas refSpace levels refLevelNum
                                 >> return True)
     onDestroy window mainQuit
     -------------
     mainGUI


startSpace :: (WidgetClass w) => w -> IORef Space -> IO ()
startSpace canvas refSpace = do
            flip timeoutAdd timePerTick $ do
                (wWidth, wHeight) <- widgetGetSize canvas
                (mX, mY) <- widgetGetPointer canvas
                let (wWidth', wHeight') =
                        (realToFrac wWidth, realToFrac wHeight)
                let (mX', mY') =
                        (realToFrac mX, realToFrac mY)
                modifyIORef refSpace $ \s ->
                        spaceNext $
                        updateController (mX' / wWidth') (mY' / wHeight') s
                widgetQueueDraw canvas
                ((== SSRunning) . sStatus) `liftM` readIORef refSpace
            return ()


doClick :: (WidgetClass w) => w -> IORef Space -> Level -> IORef Int -> IO ()
doClick canvas refSpace levels refNum = do
        status <- sStatus `liftM` readIORef refSpace
        n <- readIORef refNum
        case status of
             SSCrashed -> do
                    let reload = levelToSpace levels n
                    -- wäre es besser, wie folgt zu tun ?
                    -- writeIORef refSpace reload {sStatus = SSRunning}
                    -- startSpace canvas refSpace
                    writeIORef refSpace reload {sStatus = SSReady}
                    widgetQueueDraw canvas
             SSReady   -> do
                    modifyIORef refSpace $ \s ->
                           s {sStatus = SSRunning}
                    startSpace canvas refSpace

             SSDone   -> do
                    writeIORef refSpace $ levelToSpace levels (n + 1)
                    print n
                    writeIORef refNum (n + 1)
                    saveLevelNum $ n + 1
                    widgetQueueDraw canvas

             SSInfo _ _ next -> do
                    modifyIORef refSpace $ \s -> s {sStatus = next}
                    widgetQueueDraw canvas

             otherwise -> return()
 

doExpose :: (WidgetClass w) => w -> IORef Space -> IO ()
doExpose widget refSpace = do -- IO Monad
      space <- readIORef refSpace
      drawWindow <- widgetGetDrawWindow widget
      (nW, nH) <- widgetGetSize widget
      let (fW,fH) = (realToFrac nW, realToFrac nH)
      -- ruf `drawSpace` vom Modul Space auf
      let render = drawSpace space fW fH
      renderWithDrawable drawWindow render
