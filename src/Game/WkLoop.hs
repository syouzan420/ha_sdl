module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer)
import SDL.Time (delay)
import qualified MyData as MD
import Game.WkDraw (wkDraw)
import Game.WkEvent (wkInput,makeWkTextData)
import Game.WkData (Waka(..),Input(..),delayTime)

wkLoop :: MonadIO m => Renderer -> [Font] -> S.StateT Waka m () 
wkLoop re fonts = do 
  inp <- wkInput
  wk <- S.get
  let (texWk,tpsWk) = (tex wk,tps wk) 
      textData = makeWkTextData wk
      isShowing = tpsWk <= (T.length texWk)
  when isShowing $ wkDraw re fonts textData wk
  let (_,_,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
  let nscr = MD.scr lAtr
  let nwk = wk{tps=if isShowing then tpsWk+1 else tpsWk, scr=nscr}
  S.put nwk
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts
  
