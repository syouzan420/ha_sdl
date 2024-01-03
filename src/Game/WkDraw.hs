{-# LANGUAGE OverloadedStrings #-}
module Game.WkDraw (wkDraw) where

import Control.Monad.IO.Class (MonadIO)
import SDL.Video (Renderer)
import SDL.Video.Renderer (present)
import SDL.Font (Font)
import MySDL.MyDraw (initDraw,textsDraw)
import Game.WkData (Waka(..))
import MyData (TextData,WMode(..))

wkDraw :: (MonadIO m) => Renderer -> [Font] -> TextData -> Waka -> m ()
wkDraw re fonts textData wk = do
  initDraw re
  textsDraw re fonts T True False (tps wk) ["."] textData
  present re

