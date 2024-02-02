module Game.WkLoop (wkLoop) where

import qualified Control.Monad.State.Strict as S
import Control.Monad (unless,when)
import Control.Monad.IO.Class (MonadIO,liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified SDL.Mixer as M
import Data.Maybe (fromMaybe)
import SDL.Font (Font)
import SDL.Video.Renderer (Renderer,Surface)
import SDL.Vect (V2(..))
import SDL.Time (delay)
import qualified MyData as MD
import MyAction (getCid)
import General (getLastChar,toList)
import Game.WkDraw (wkDraw)
import Game.WkEvent (exeEvent)
import Game.WkAction (wkInput,makeWkTextData)
import Game.WkData (Waka(..),Input(..),IMode(..),Direction(..),Cha(..),Pos,GMap,MProp(..)
                   ,delayTime,visibleMapSize,plDelay,chaDelay,defaultMapProp)

wkLoop :: MonadIO m => Renderer -> [Font] -> [[Surface]]
                                      -> [M.Music] -> S.StateT Waka m () 
wkLoop re fonts surfs muses = do 
  inp <- wkInput
  wk <- S.get
  let (imsWk,impWk) = (ims wk,imp wk)
  let isMusicOn = imsWk && (not impWk) 
  let isMusicOff = imsWk && impWk
  when isMusicOn $ M.playMusic M.Forever (muses!!(mfn wk))
  _ <- if isMusicOff then  M.fadeOutMusic 1000 else return False
  S.put (wk{ims=False,imp=if isMusicOn then True else if isMusicOff then False else impWk})
  let mdiWk = mdi wk
  case mdiWk of
    TXT -> textMode re fonts surfs inp
    PLY -> mapMode re fonts surfs inp
    _ -> return ()
  delay delayTime
  unless (inp==Es) $ wkLoop re fonts surfs muses

textMode :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> Input -> S.StateT Waka m ()
textMode re fonts surfs inp = do
  wk <- S.get
  let (texWk,stxWk,tpsWk,tmdWk,chsWk) 
          = (tex wk,stx wk,tps wk,tmd wk,chs wk) 
      lch = getLastChar (T.take (tpsWk+1) texWk)
      isStop = lch == '。'
      isEvent = lch == '\\'
      isMap = lch == '~'
      isDialog = tpsWk < (T.length texWk)
      isShowing = isDialog && (not isStop)
      eventText = if isEvent then getTargetText getEventLength tpsWk texWk else T.empty
      addTps = if isShowing then calcTps tpsWk texWk else tpsWk
      addText = case lch of
                  '。'  -> T.empty
                  '\\' -> T.empty
                  '~' -> T.empty
                  ';'  -> T.singleton ';'<>getTargetText getComLength tpsWk texWk
                  _    -> T.singleton lch
      nStx = if isShowing then stxWk <> addText else stxWk
      ntps = if isShowing then tpsWk+addTps else tpsWk
      nwk = wk{stx=nStx,tps=ntps}
      textData = makeWkTextData nwk
  wkDraw re fonts surfs textData nwk
  when isEvent $ liftIO $ print eventText
  let (_,_,lAtr,_) = if null textData then (False,T.empty,MD.initAttr,[]) 
                                      else last textData 
  let nscr = MD.scr lAtr
  let isStart = isStop && inp==Sp
  let nstx' = if isStart && tmdWk==0 then T.empty else nStx
  let ntps' = if isStart then ntps+1 else ntps 
  let nmsz = if tmdWk==0 then V2 0 0 else visibleMapSize 
  let nchs = map (\(cr,dl) 
        -> cr{cac = let cacCr = cac cr in if cacCr==dl*2 then 0 else cacCr+1})
                                                               (zip chsWk chaDelay)
  let nmdi = if isMap then PLY else TXT 
  let nwk' = nwk{stx=nstx', tps=ntps', scr=nscr, msz=nmsz, mdi=nmdi, chs=nchs}
  S.put nwk'
  when isEvent $ exeEvent eventText

mapMode :: (MonadIO m) => Renderer -> [Font] -> [[Surface]] -> Input -> S.StateT Waka m ()
mapMode re fonts surfs inp = do
  wk <- S.get
  let (chsWk,plnWk,mpsWk,gmpWk) 
            = (chs wk,pln wk,mps wk,gmp wk) 
      textData = makeWkTextData wk
      chP = chsWk!!plnWk
      (pdrCh,ppsCh,prpCh,pacCh) = (cdr chP, cps chP, crp chP, cac chP)
      npdr = case inp of
                Ri -> East
                Up -> North
                Lf -> West
                Dn -> South
                _  -> pdrCh
      iStop = inp/=Ri && inp/=Up && inp /=Lf && inp/=Dn
      (nmps,(npps,nprp)) = charaMove True iStop gmpWk mpsWk npdr ppsCh prpCh 
      nchP = chP{cdr=npdr,cps=npps,crp=nprp} 
      chs' = toList chsWk plnWk nchP
      nchs = map (\(cr,dl) 
        -> cr{cac = let cacCr = cac cr in if cacCr==dl*2 then 0 else cacCr+1})
                                                               (zip chs' chaDelay)
      nwk = wk{chs=nchs}
  wkDraw re fonts surfs textData nwk
  S.put nwk

type MapPos = Pos
type ChaPos = Pos
type ChaRPos = Pos
type IsPlayer = Bool

charaMove :: IsPlayer -> Bool -> GMap -> MapPos -> Direction 
                    -> ChaPos -> ChaRPos -> (MapPos,(ChaPos,ChaRPos)) 
charaMove ip ist gm mpos@(V2 x y) dr cpos@(V2 a b) crps@(V2 p q) =  
  let isFr = isFree gm
      ts = 32
      du = 8
      canMove = not ist && case dr of
        East -> (p==0 && isFr (V2 (a+1) b) 
                      && (q==0 || (q>0 && (isFr (V2 (a+1) (b+1)))) 
                               || (q<0 && (isFr (V2 (a+1) (b-1)))))) || p/=0  
        North -> (q==0 && isFr (V2 a (b-1))
                       && (p==0 || (p>0 && (isFr (V2 (a+1) (b-1))))
                                || (p<0 && (isFr (V2 (a-1) (b-1)))))) || q/=0
        West -> (p==0 && isFr (V2 (a-1) b)
                      && (q==0 || (q>0 && (isFr (V2 (a-1) (b+1))))
                               || (q<0 && (isFr (V2 (a-1) (b-1)))))) || p/=0
        South -> (q==0 && isFr (V2 a (b+1))
                       && (p==0 || (p>0 && (isFr (V2 (a+1) (b+1))))
                                || (p<0 && (isFr (V2 (a-1) (b+1)))))) || q/=0
      (V2 dx dy) = if canMove then case dr of 
              East -> V2 du 0; North -> V2 0 (-du); West -> V2 (-du) 0; South -> V2 0 du 
                              else V2 0 0
      (V2 np nq) = crps + (V2 dx dy) 
      ts' = fromIntegral ts
      da = if (mod np ts' == 0) && np/=0 then div np ts' else 0
      db = if (mod nq ts' == 0) && nq/=0 then div nq ts' else 0
      ncpos = cpos + (V2 da db)
      np' = if (abs np==ts') then 0 else np
      nq' = if (abs nq==ts') then 0 else nq
   in (mpos,(ncpos,(V2 np' nq'))) 

isFree :: GMap -> ChaPos -> Bool
isFree gm (V2 a b) =
  let mProp = defaultMapProp!!(read [((gm!!fromIntegral b)!!fromIntegral a)])
   in mProp/=Bl

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
