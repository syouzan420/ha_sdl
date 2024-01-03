module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer)
import SDL.Time (delay)
import qualified MyData as MD
import MyAction (getCid)
import General (getLastChar)
import Game.WkDraw (wkDraw)
import Game.WkEvent (wkInput,makeWkTextData)
import Game.WkData (Waka(..),Input(..),delayTime)

wkLoop :: MonadIO m => Renderer -> [Font] -> S.StateT Waka m () 
wkLoop re fonts = do 
  inp <- wkInput
  wk <- S.get
  let (texWk,tpsWk) = (tex wk,tps wk) 
      textData = makeWkTextData wk
      (_,ltx,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
      lch = getLastChar ltx 
      isStop = lch == '.'
      isShowing = tpsWk <= (T.length texWk) && (not isStop)
  when isShowing $ wkDraw re fonts textData wk
  let nscr = MD.scr lAtr
  let ntps = if isShowing then calcTps (tpsWk+1) texWk else tpsWk
  let ntps' = if isStop && inp==Sp then ntps+1 else ntps
  let nwk = wk{tps=ntps', scr=nscr}
  S.put nwk
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts
  
calcTps :: Int -> Text -> Int 
calcTps tpsWk texWk =
  let tmpText = T.take tpsWk texWk
      ch = getLastChar tmpText 
   in case ch of
        ';' -> tpsWk + getComLength (T.drop tpsWk texWk)
        _   -> tpsWk

getComLength :: Text -> Int
getComLength tx =
  let (cidWk,_) = getCid tx 
   in toSpace cidWk tx

toSpace :: Int -> Text -> Int
toSpace (-1) _ = (-1) 
toSpace i tx =
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx)
      ni = if ch==' ' then i-1 else i
   in if tx==T.empty then 0 else 1 + toSpace ni txs
