module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
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
  let (texWk,stxWk,tpsWk) = (tex wk,stx wk,tps wk) 
      lch = getLastChar (T.take (tpsWk+1) texWk)
      isStop = lch == '.'
      isEvent = lch == '\\'
      isShowing = tpsWk < (T.length texWk) && (not isStop)
      eventText = if isEvent then getEventText tpsWk texWk else T.empty
      addTps = if isShowing then calcTps tpsWk texWk else tpsWk
      addText = case lch of
                  '.'  -> T.empty
                  '\\' -> T.empty
                  ';'  -> T.singleton ';'<>getComText tpsWk texWk
                  _    -> T.singleton lch
      nStx = if isShowing then stxWk <> addText else stxWk
      ntps = if isShowing then tpsWk+addTps else tpsWk
      nwk = wk{stx=nStx,tps=ntps}
      textData = makeWkTextData nwk
  when isShowing $ wkDraw re fonts textData nwk
  when isEvent $ liftIO $ putStrLn (T.unpack eventText)
  let (_,_,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
  let nscr = MD.scr lAtr
  let ntps' = if isStop && inp==Sp then ntps+1 else ntps 
  let nwk' = nwk{tps=ntps', scr=nscr}
  S.put nwk'
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts
  
calcTps :: Int -> Text -> Int 
calcTps tpsWk texWk =
  let beforeTpsText = T.take (tpsWk+1) texWk
      afterTpsText = T.drop (tpsWk+1) texWk
      ch = getLastChar beforeTpsText 
   in case ch of
        ';' -> 1 + getComLength afterTpsText 
        '\\' -> getEventLength afterTpsText 
        _   -> 1 

getEventText :: Int -> Text -> Text
getEventText tpsWk texWk =
  let afterTpsText = T.drop (tpsWk+1) texWk
      eventLength = getEventLength afterTpsText
   in T.take eventLength afterTpsText 

getEventLength :: Text -> Int
getEventLength tx =
  let txs = T.lines tx
      num = if null txs then 0 else T.length (head txs)
   in if length txs < 2 then num else num+1

getComText :: Int -> Text -> Text
getComText tpsWk texWk =
  let afterTpsText = T.drop (tpsWk+1) texWk
      comLength = getComLength afterTpsText
   in T.take comLength afterTpsText 

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
