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
                 -- windowDefaultWidth := 640,
                 -- windowDefaultHeight := 480,
                 windowDefaultWidth := 480,
                 windowDefaultHeight := 480,
                 containerBorderWidth := 5,
                 containerChild := canvas
                 ]


     widgetModifyBg canvas StateNormal (Color 0 0 0)
     widgetShowAll window

     global <- initSpace

     onExpose canvas (\x -> doExpose canvas global)

     -- timeoutAdd ( widgetQueueDraw canvas
                  -- >> sRunning `liftM` readIORef global)
                -- 50

     timeoutAdd ( spaceNext global >> widgetQueueDraw canvas
                  >> sRunning `liftM` readIORef global)
                50

     onDestroy window mainQuit
     mainGUI

-- bNext entscheidet, ob der nächste Zustand gerechnet wird
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
