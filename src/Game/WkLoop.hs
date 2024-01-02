module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer)
import SDL.Time (delay)
import MySDL.MyDraw (wkDraw)
import Game.WkEvent (wkInput)
import Game.WkData (Waka(..),Input(..),delayTime)

wkLoop :: MonadIO m => Renderer -> [Font] -> S.StateT Waka m () 
wkLoop re fonts = do 
  inp <- wkInput
  wk <- S.get
  let (texWk,tpsWk) = (tex wk,tps wk) 
  let nwk = wk{tps=tpsWk+1}
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts
  
