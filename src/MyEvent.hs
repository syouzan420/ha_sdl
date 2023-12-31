{-# LANGUAGE OverloadedStrings #-}
module MyEvent (inputEvent) where

import MySDL.MyInput (myInput)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import qualified Control.Monad.State.Strict as S
import qualified Data.Text as T
import MyData (State(..),Active(..),Attr(..),Coding(..),Jumping(..),Modif(..),WMode(..),EMode(..),FMode(..),Input(..),initYokoPos,initTatePos,colorPallet)
import MyLib (tpsForRelativeLine,locToIndex,toDotPos,addMidDots,selectNearest,textIns,lastTps,takeCurrentLine,deleteCurrentLine,headTps)
import Mana.Mana (evalCode,taiyouMn,Mn(..),Yo(..),Dtype(..),preDef,userDef)
import SDL.Input.Keyboard.Codes

inputEvent :: S.StateT State IO Input
inputEvent = do
  st <- S.get
  let (actSt,cdnSt) = (act st, cdn st) 
      (texSt,etxSt,dtsSt,tpsSt,dfnSt,comSt,atrSt,emdSt,cplSt,ifmSt,iskSt) =
          (tex actSt,etx actSt,dts actSt,tps actSt,dfn cdnSt
          ,com st,atr st,emd st,cpl st,ifm st,isk st)
  (kc,md,it,mps,isc,ised) <- myInput  -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isKeyPressed = kc/=KeycodeUnknown
      isMousePressed = mps/=V2 (-1) (-1)
      isQuit = kc==KeycodeEscape   -- ESC Key

      isNor = emdSt==Nor
      isIns = emdSt==Ins
      isRet = kc==KeycodeReturn

      ----normal mode

      isUp = (kc==KeycodeK && isNor) || kc==KeycodeUp
      isDown = (kc==KeycodeJ && isNor) || kc==KeycodeDown
      isLeft = (kc==KeycodeH && isNor) || kc==KeycodeLeft
      isRight = (kc==KeycodeL && isNor) || kc==KeycodeRight

      isToIns = kc==KeycodeI && isNor

      ----normal with shift key

      isFarForward = kc==KeycodeF && md==Shf && isNor
      isFarBack = kc==KeycodeB && md==Shf && isNor

      ----insert mode

      isToNor = isExit && isIns

      ----with ctrl key

      isNewFile = kc==KeycodeN && md==Ctr
      isLoadFile = kc==KeycodeL && md==Ctr
      isLoadRecentFile = kc==KeycodeR && md==Ctr

      isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)

      isExit = kc==KeycodeLeftBracket && md==Ctr

      isTglOsd = kc==KeycodeO && md==Ctr
      isTglMin = kc==KeycodeM && md==Ctr
      isTglFmt = kc==KeycodeF && md==Ctr

      isFontPlus = kc==KeycodeEquals && md==Ctr
      isFontMinus = kc==KeycodeMinus && md==Ctr

      isExeCode = kc==KeycodeE && md==Ctr

      isDrawClear = kc==KeycodeD && md==Ctr
      isTglColor = kc==KeycodeC && md==Ctr


      isJump = ifmSt && isRet && (sjn.jmp) atrSt>=0
      isJBak = ifmSt && isBS

      isSkkEdit = it==T.empty && kc/=KeycodeRShift && kc/=KeycodeLShift 
                              && kc/=KeycodeUnknown && md==Shf

      isBS = (kc==KeycodeBackspace && not iskSt) || (isNor && kc==KeycodeX)
      isDeleteLine = not (null comSt) && last comSt=='d' && kc==KeycodeD 
      
      isCom = md==Alt
      comName = case kc of
                  KeycodeR -> ";rb "
                  _        -> T.empty

      (wm,fm,lw,fjpAt,fszAt,V2 ww wh,V4 mr mt ml mb,scrAt@(V2 sx sy)) =
         (wmd atrSt,fmd atrSt,lnw atrSt,(fjp.jmp) atrSt
         ,fsz atrSt,wsz atrSt,mgn atrSt,scr atrSt)
      tLen = T.length texSt
      seeLines = fromIntegral$if wm==T then (ww-mr-ml) `div` lw else (wh-mt-mb) `div` lw
      tpsPreLine = tpsForRelativeLine atrSt texSt (-1) tpsSt
      tpsNextLine = tpsForRelativeLine atrSt texSt 1 tpsSt
      tpsFarBack = tpsForRelativeLine atrSt texSt (-seeLines) tpsSt
      tpsFarForward = tpsForRelativeLine atrSt texSt seeLines tpsSt

      codeMana 
        | isExeCode = evalCode (preDef++[(User,userDef++dfnSt)]) (takeCurrentLine tpsSt texSt)
        | otherwise = Mn "" Moz
      (ta,yo) = taiyouMn codeMana
      codeResult 
        | isExeCode && yo==Io = T.empty 
        | isExeCode = "\n"<>(T.pack.show) codeMana 
        | otherwise = T.empty
      
      nit = if isIns && isRet && it==T.empty then "\n" else it
      centerLineNum = if wm==T then (ww-mr-ml) `div` lw `div` 2  + sx `div` lw 
                               else (wh-mt-mb) `div` lw `div` 2 - sy `div` lw
      centerIndex = locToIndex atrSt texSt (fromIntegral centerLineNum,0)
      nsjn = selectNearest centerIndex (map fst fjpAt)
      nscr
        | ifmSt && wm==T && isLeft = scrAt+V2 lw 0
        | ifmSt && wm==T && isRight = scrAt-V2 lw 0
        | ifmSt && wm==Y && isUp = scrAt+V2 0 lw
        | ifmSt && wm==Y && isDown = scrAt-V2 0 lw
        | otherwise = scrAt
      nfsz
        | isFontPlus = fszAt + 1
        | isFontMinus = fszAt - 1
        | otherwise = fszAt
      natr
        | isTglDir = if wm==T then atrSt{gps=initYokoPos,wmd=Y,scr=V2 0 0} 
                              else atrSt{gps=initTatePos,wmd=T,scr=V2 0 0} 
        | isTglOsd = if fm==Ost then atrSt{fmd=Got} else atrSt{fmd=Ost}
        | isTglMin = if fm==Min then atrSt{fmd=Got} else atrSt{fmd=Min}
        | otherwise = atrSt{scr=nscr,jmp=(jmp atrSt){sjn=nsjn},fsz=nfsz}
      netx 
        | ised = it 
        | isIns && not ised && it/=T.empty = T.empty
        | otherwise = etxSt
      ncpl
        | isTglColor = if cplSt==length colorPallet - 1 then 0 else cplSt+1 
        | otherwise = cplSt
      ntps
        | ifmSt = tpsSt
        | isFarForward = tpsFarForward
        | isFarBack = tpsFarBack
        | isUp = if wm==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
        | isDown = if wm==T then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
        | isLeft = if wm==Y then if tpsSt==0 then 0 else tpsSt-1 else tpsNextLine
        | isRight = if wm==Y then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
        | isCom = tpsSt + T.length comName 
        | isExeCode = lastTps tpsSt texSt + T.length codeResult
        | isIns && nit/=T.empty && not ised = tpsSt + T.length nit 
        | isBS = if tpsSt>0 then tpsSt-1 else tpsSt
        | isDeleteLine = headTps tpsSt texSt
        | otherwise = tpsSt
      nemd
        | isToIns = Ins 
        | isToNor = Nor 
        | otherwise = emdSt
      ntex
        | ifmSt = texSt
        | isBS && tpsSt>0 = T.take (tpsSt-1) texSt <> T.drop tpsSt texSt
        | isDeleteLine = deleteCurrentLine tpsSt texSt
        | isCom = textIns comName tpsSt texSt
        | isExeCode = textIns codeResult (lastTps tpsSt texSt) texSt 
        | isIns && not ised = textIns nit tpsSt texSt 
        | otherwise = texSt
      ndts 
        | isDrawClear = []
        | isMousePressed && isc && not (null dtsSt) = 
          dtsSt ++ addMidDots (last dtsSt) (toDotPos mps scrAt,cplSt) ++ [(toDotPos mps scrAt,cplSt)]
        | isMousePressed = dtsSt++[(toDotPos mps scrAt,cplSt)]  
        | otherwise = dtsSt
      ncod
        | isExeCode && yo==Io = if '\n' `elem` ta then lines ta else [ta] 
        | otherwise = [] 
      ncom
        | isExit || texSt /= ntex = "" 
        | isNor = comSt ++ T.unpack it 
        | otherwise = comSt
      nifm
        | isTglFmt = not ifmSt
        | otherwise = ifmSt
      nisk
        | isSkkEdit = True
        | iskSt && it/=T.empty = False 
        | otherwise = iskSt
      ninp
        | isNewFile = NFL
        | isLoadFile = LFL
        | isLoadRecentFile = LRF
        | isJump = JMP 
        | isJBak = JBK
        | isExeCode && yo==Io = EXE
        | isQuit = QIT
        | isKeyPressed = PKY
        | isMousePressed = PMO
        | otherwise = NON
      nactSt = actSt{tex=ntex,etx=netx,dts=ndts,tps=ntps}
      nst = st{act=nactSt,cdn=cdnSt{cod=ncod},com=ncom,atr=natr,emd=nemd,cpl=ncpl,ifm=nifm,isk=nisk}
  S.put nst
  return ninp

