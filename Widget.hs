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

     global <- initSpace

     onExpose canvas (\x -> doExpose canvas global)

     flip timeoutAdd timePerTick $ do
                     (wWidth, wHeight) <- widgetGetSize canvas
                     (mX, mY) <- widgetGetPointer canvas
                     let (wWidth', wHeight') =
                             (realToFrac wWidth, realToFrac wHeight)
                     let (mX', mY') =
                             (realToFrac mX, realToFrac mY)
                     updateController (mX' / wWidth') (mY' / wHeight') global
                     spaceNext global
                     widgetQueueDraw canvas
                     sRunning `liftM` readIORef global

     onDestroy window mainQuit
     mainGUI


doExpose :: (WidgetClass w) => w -> IORef Space -> IO Bool
doExpose widget refSpace = do -- IO Monad
      space <- readIORef refSpace
      drawWindow <- widgetGetDrawWindow widget
      (nW, nH) <- widgetGetSize widget
      let (fW,fH) = (realToFrac nW, realToFrac nH)
      -- ruf `drawSpace` vom Modul Space auf
      render <- drawSpace space fW fH
      renderWithDrawable drawWindow render
      return True
