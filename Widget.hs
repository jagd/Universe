module Widget where

import Control.Monad (liftM)
import Data.IORef

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo

import Space

main :: IO ()
main= do
     initGUI
     window <- windowNew
     canvas <- drawingAreaNew
     set window [windowTitle := "The Space",
                 windowDefaultWidth := 600,
                 windowDefaultHeight := 600,
                 containerBorderWidth := 5,
                 containerChild := canvas
                 ]
     widgetModifyBg canvas StateNormal (Color 0 0 0)
     widgetShowAll window
     -------------
     -- Events: --
     -------------
     refSpace <- newIORef initSpace
     onExpose canvas (\x -> doExpose canvas refSpace >> return True)
     onButtonPress canvas (\x -> doClick canvas refSpace >> return True)
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
                updateController (mX' / wWidth') (mY' / wHeight') refSpace
                spaceNext refSpace
                widgetQueueDraw canvas
                ((== SSRunning) . sStatus) `liftM` readIORef refSpace
            return ()


doClick :: (WidgetClass w) => w -> IORef Space -> IO ()
doClick canvas refSpace = do
        status <- sStatus `liftM` readIORef refSpace
        case status of
             SSCrashed -> do
                          writeIORef refSpace
                                     initSpace {sStatus = SSRunning}
                          startSpace canvas refSpace
                          return ()
             SSReady   -> do
                          writeIORef refSpace
                                     initSpace {sStatus = SSRunning}
                          startSpace canvas refSpace
                          return ()
             otherwise -> return()
 

doExpose :: (WidgetClass w) => w -> IORef Space -> IO ()
doExpose widget refSpace = do -- IO Monad
      space <- readIORef refSpace
      drawWindow <- widgetGetDrawWindow widget
      (nW, nH) <- widgetGetSize widget
      let (fW,fH) = (realToFrac nW, realToFrac nH)
      -- ruf `drawSpace` vom Modul Space auf
      render <- drawSpace space fW fH
      renderWithDrawable drawWindow render
