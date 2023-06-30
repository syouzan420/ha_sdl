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
      isUp = kc==KeycodeK && isNor 
      isDown = kc==KeycodeJ && isNor
      isLeft = kc==KeycodeH && isNor
      isRight = kc==KeycodeL && isNor
      isToIns = kc==KeycodeI && isNor
      isToNor = kc==KeycodeLeftBracket && md==Ctr && isIns
      isTglOsd = kc==KeycodeO && md==Ctr
      texSt = tex st
      atrSt = atr st
      tpsSt = tps st
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
        | isUp = if wm==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
        | isDown = if wm==T then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
        | isLeft = if wm==Y then if tpsSt==0 then 0 else tpsSt-1 else tpsNextLine
        | isRight = if wm==Y then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
        | isIns && nit/=T.empty = tpsSt + T.length nit 
        | otherwise = tpsSt
      nemd
        | isToIns = Ins 
        | isToNor = Nor 
        | otherwise = emdSt
      ntex
        | isIns = T.take tpsSt texSt <> nit <> T.drop tpsSt texSt 
        | otherwise = texSt
      nst = st{tex=ntex,atr=natr,tps=ntps,emd=nemd}
  return (nst,isKeyPressed,isQuit)
