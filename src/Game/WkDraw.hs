module Game.WkDraw (wkDraw) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO)
import SDL.Video (Renderer)
import SDL.Video.Renderer (present)
import SDL.Font (Font)
import MySDL.MyDraw (initDraw,textsDraw)
import Game.WkData (Waka(..),Size)
import MyData (TextData,WMode(..))

type MapSize = Size
type IsTextShowing = Bool

wkDraw :: (MonadIO m) => Renderer -> [Font] -> TextData 
                            -> MapSize -> IsTextShowing -> Waka -> m ()
wkDraw re fonts textData msize ish wk = do
  initDraw re
  when ish $ textsDraw re fonts T True False (tps wk) textData
  present re

