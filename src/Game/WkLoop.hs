module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Surface)
import SDL.Vect (V2(..))
import SDL.Time (delay)
import qualified MyData as MD
import MyAction (getCid)
import General (getLastChar)
import Game.WkDraw (wkDraw)
import Game.WkEvent (exeEvent)
import Game.WkAction (wkInput,makeWkTextData)
import Game.WkData (Waka(..),Input(..),delayTime)

wkLoop :: MonadIO m => Renderer -> [Font] -> [[Surface]] -> S.StateT Waka m () 
wkLoop re fonts surfs = do 
  inp <- wkInput
  wk <- S.get
  let (texWk,stxWk,tpsWk,tmdWk) = (tex wk,stx wk,tps wk,tmd wk) 
      lch = getLastChar (T.take (tpsWk+1) texWk)
      isStop = lch == '。'
      isEvent = lch == '\\'
      isShowing = tpsWk < (T.length texWk) && (not isStop)
      eventText = if isEvent then getTargetText getEventLength tpsWk texWk else T.empty
      addTps = if isShowing then calcTps tpsWk texWk else tpsWk
      addText = case lch of
                  '。'  -> T.empty
                  '\\' -> T.empty
                  ';'  -> T.singleton ';'<>getTargetText getComLength tpsWk texWk
                  _    -> T.singleton lch
      nStx = if isShowing then stxWk <> addText else stxWk
      ntps = if isShowing then tpsWk+addTps else tpsWk
      nwk = wk{stx=nStx,tps=ntps}
      textData = makeWkTextData nwk
  when isShowing $ wkDraw re fonts surfs textData nwk
  when isEvent $ liftIO $ print eventText
  let (_,_,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
  let nscr = MD.scr lAtr
  let isStart = isStop && inp==Sp
  let nstx' = if isStart && tmdWk==0 then T.empty else nStx
  let ntps' = if isStart then ntps+1 else ntps 
  let nmsz = if tmdWk==0 then V2 0 0 else V2 5 5
  let nwk' = nwk{stx=nstx', tps=ntps', scr=nscr, msz=nmsz}
  S.put nwk'
  when isEvent $ exeEvent eventText
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts surfs
  
calcTps :: Int -> Text -> Int 
calcTps tpsWk texWk =
  let beforeTpsText = T.take (tpsWk+1) texWk
      afterTpsText = T.drop (tpsWk+1) texWk
      ch = getLastChar beforeTpsText 
   in case ch of
        ';' -> 1 + getComLength afterTpsText 
        '\\' -> 1 + getEventLength afterTpsText 
        _   -> 1 

getTargetText :: (Text -> Int) -> Int -> Text -> Text
getTargetText getLengthF tpsWk texWk =
  let afterTpsText = T.drop (tpsWk+1) texWk
      textLength = getLengthF afterTpsText
   in T.take textLength afterTpsText 

getEventLength :: Text -> Int
getEventLength tx =
  let txs = T.lines tx
      num = if null txs then 0 else T.length (head txs)
   in if length txs < 2 then num else num+1

getComLength :: Text -> Int
getComLength tx =
  let (cidWk,_) = getCid tx 
   in spaceNum cidWk tx

spaceNum :: Int -> Text -> Int
spaceNum (-1) _ = (-1) 
spaceNum i tx =
  let (ch,txs) = fromMaybe ('0',T.empty) (T.uncons tx)
      ni = if ch==' ' then i-1 else i
   in if tx==T.empty then 0 else 1 + spaceNum ni txs
