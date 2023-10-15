{-# LANGUAGE OverloadedStrings #-}
module MyEvent (inputEvent) where

import MySDL.MyInput (myInput)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import qualified Data.Text as T
import MyData (State(..),Attr(..),Modif(..),WMode(..),EMode(..),Input(..),initYokoPos,initTatePos,colorPallet)
import MyLib (tpsForRelativeLine,locToIndex,toDotPos,addMidDots,selectNearest)
import SDL.Input.Keyboard.Codes

inputEvent :: State -> IO (State,Input)
inputEvent st@(State texSt dtsSt atrSt _ tpsSt _ emdSt cplSt ifmSt _ iskSt _) = do
  (kc,md,it,mps,isc) <- myInput    -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isKeyPressed = kc/=KeycodeUnknown
      isMousePressed = mps/=V2 (-1) (-1)
      isQuit = kc==KeycodeEscape   -- ESC Key
      isNewFile = kc==KeycodeN && md==Ctr
      isLoadFile = kc==KeycodeL && md==Ctr
      isJump = ifmSt && isRet && sjn atrSt>=0
      isJBak = ifmSt && isBS

      isSkkEdit = it==T.empty && kc/=KeycodeRShift && kc/=KeycodeLShift && kc/=KeycodeUnknown && md==Shf
      isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)

      isNor = emdSt==Nor
      isIns = emdSt==Ins
      isRet = kc==KeycodeReturn
      isUp = (kc==KeycodeK && isNor) || kc==KeycodeUp
      isDown = (kc==KeycodeJ && isNor) || kc==KeycodeDown
      isLeft = (kc==KeycodeH && isNor) || kc==KeycodeLeft
      isRight = (kc==KeycodeL && isNor) || kc==KeycodeRight
      isFarForward = kc==KeycodeF && md==Shf && isNor
      isFarBack = kc==KeycodeB && md==Shf && isNor

      isToIns = kc==KeycodeI && isNor
      isToNor = kc==KeycodeLeftBracket && md==Ctr && isIns
      isTglOsd = kc==KeycodeO && md==Ctr
      isTglFmt = kc==KeycodeF && md==Ctr

      isExeCode = kc==KeycodeE && md==Ctr

      isBS = (kc==KeycodeBackspace && not iskSt) || (isNor && kc==KeycodeX)
      isCom = md==Alt
      isDrawClear = kc==KeycodeD && md==Ctr
      isTglColor = kc==KeycodeC && md==Ctr

      comName = case kc of
                  KeycodeR -> ";rb "
                  _        -> T.empty
      tLen = T.length texSt
      wm = wmd atrSt
      os = ios atrSt
      lw = lnw atrSt
      fjpAt = fjp atrSt
      V2 ww wh = wsz atrSt
      V4 mr mt ml mb = mgn atrSt
      seeLines = fromIntegral$if wm==T then (ww-mr-ml) `div` lw else (wh-mt-mb) `div` lw
      scrAt@(V2 sx sy) = scr atrSt
      ncpl = if isTglColor then if cplSt==length colorPallet - 1 then 0 else cplSt+1 else cplSt
      tpsPreLine = tpsForRelativeLine atrSt texSt (-1) tpsSt
      tpsNextLine = tpsForRelativeLine atrSt texSt 1 tpsSt
      tpsFarBack = tpsForRelativeLine atrSt texSt (-seeLines) tpsSt
      tpsFarForward = tpsForRelativeLine atrSt texSt seeLines tpsSt
      nit = if isIns && isRet then "\n" else it
      textIns tx = T.take tpsSt texSt <> tx <> T.drop tpsSt texSt 
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
      natr
        | isTglDir = if wm==T then atrSt{gps=initYokoPos,wmd=Y,scr=V2 0 0} 
                              else atrSt{gps=initTatePos,wmd=T,scr=V2 0 0} 
        | isTglOsd = if os then atrSt{ios=False} else atrSt{ios=True}
        | otherwise = atrSt{scr=nscr,sjn=nsjn}
      ntps
        | ifmSt = tpsSt
        | isFarForward = tpsFarForward
        | isFarBack = tpsFarBack
        | isUp = if wm==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
        | isDown = if wm==T then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
        | isLeft = if wm==Y then if tpsSt==0 then 0 else tpsSt-1 else tpsNextLine
        | isRight = if wm==Y then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
        | isCom = tpsSt + T.length comName 
        | isIns && nit/=T.empty = tpsSt + T.length nit 
        | isBS = if tpsSt>0 then tpsSt-1 else tpsSt
        | otherwise = tpsSt
      nemd
        | isToIns = Ins 
        | isToNor = Nor 
        | otherwise = emdSt
      ntex
        | ifmSt = texSt
        | isBS && tpsSt>0 = T.take (tpsSt-1) texSt <> T.drop tpsSt texSt
        | isCom = textIns comName 
        | isIns = textIns nit 
        | otherwise = texSt
      ndts 
        | isDrawClear = []
        | isMousePressed && isc && not (null dtsSt) = 
          dtsSt ++ addMidDots (last dtsSt) (toDotPos mps scrAt,cplSt) ++ [(toDotPos mps scrAt,cplSt)]
        | isMousePressed = dtsSt++[(toDotPos mps scrAt,cplSt)]  
        | otherwise = dtsSt
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
        | isJump = JMP 
        | isJBak = JBK
        | isExeCode = EXE
        | isQuit = QIT
        | isKeyPressed = PKY
        | isMousePressed = PMO
        | otherwise = NON
      nst = st{tex=ntex,dts=ndts,atr=natr,tps=ntps,emd=nemd,cpl=ncpl,ifm=nifm,isk=nisk}
  return (nst,ninp)

