module MySDL.MyInitVideo (withMyVideo) where

import SDL.Video (createWindow, defaultWindow, windowInitialSize
                 ,createRenderer,defaultRenderer,destroyWindow)
import SDL.Video.Renderer (Surface,Renderer,Texture,createTextureFromSurface,present,freeSurface)
--import SDL.Input.Keyboard (startTextInput,stopTextInput)
--import SDL.Raw.Types (Rect(..))
import MySDL.MyDraw (initDraw)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO)
import MyData (windowSize,title)

withMyVideo :: (MonadIO m) => [Surface] -> ((Renderer,[Texture]) -> m a) -> m ()
withMyVideo imageS op = do
      window <- createWindow title (defaultWindow {windowInitialSize = windowSize})
      renderer <- createRenderer window (-1) defaultRenderer
      itexs <- mapM (createTextureFromSurface renderer) imageS
      mapM_ freeSurface imageS
      initDraw renderer
      present renderer
--      startTextInput (Rect 0 0 200 50)
      void $ op (renderer,itexs)
--      stopTextInput
      destroyWindow window
