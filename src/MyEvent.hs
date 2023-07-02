{-# LANGUAGE OverloadedStrings #-}
module MyEvent (inputEvent) where

import MySDL.MyInput (myInput)
import qualified Data.Text as T
import MyData (State(..),Attr(..),Modif(..),WMode(..),EMode(..),initYokoPos,initTatePos)
import MyAction (tpsForRelativeLine)
import SDL.Input.Keyboard.Codes

inputEvent :: State -> IO (State,Bool,Bool)
inputEvent st = do
  (kc,md,it) <- myInput    -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isKeyPressed = kc/=KeycodeUnknown
      isQuit = kc==KeycodeEscape   -- ESC Key
      isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)
      emdSt = emd st
      isNor = emdSt==Nor
      isIns = emdSt==Ins
      isRet = kc==KeycodeReturn
      isUp = (kc==KeycodeK && isNor) || kc==KeycodeUp
      isDown = (kc==KeycodeJ && isNor) || kc==KeycodeDown
      isLeft = (kc==KeycodeH && isNor) || kc==KeycodeLeft
      isRight = (kc==KeycodeL && isNor) || kc==KeycodeRight
      isToIns = kc==KeycodeI && isNor
      isToNor = kc==KeycodeLeftBracket && md==Ctr && isIns
      isTglOsd = kc==KeycodeO && md==Ctr
      isTglFmt = kc==KeycodeF && md==Ctr
      isBS = kc==KeycodeBackspace
      texSt = tex st
      atrSt = atr st
      tpsSt = tps st
      ifmSt = ifm st
      tLen = T.length texSt
      wm = wmd atrSt
      os = ios atrSt
      tpsPreLine = tpsForRelativeLine atrSt texSt (-1) tpsSt
      tpsNextLine = tpsForRelativeLine atrSt texSt 1 tpsSt
      nit = if isIns && isRet then "\n" else it
      natr
        | isTglDir = if wm==T then atrSt{gps=initYokoPos,wmd=Y} else atrSt{gps=initTatePos,wmd=T} 
        | isTglOsd = if os then atrSt{ios=False} else atrSt{ios=True}
        | otherwise = atrSt
      ntps
        | ifmSt = tpsSt
        | isUp = if wm==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
        | isDown = if wm==T then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
        | isLeft = if wm==Y then if tpsSt==0 then 0 else tpsSt-1 else tpsNextLine
        | isRight = if wm==Y then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
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
        | isIns = T.take tpsSt texSt <> nit <> T.drop tpsSt texSt 
        | otherwise = texSt
      nifm
        | isTglFmt = not ifmSt
        | otherwise = ifmSt
      nst = st{tex=ntex,atr=natr,tps=ntps,emd=nemd,ifm=nifm}
  return (nst,isKeyPressed,isQuit)
