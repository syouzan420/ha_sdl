module MyEvent (inputEvent) where

import MySDL.MyInput (myInput)
import qualified Data.Text as T
import MyData (State(..),Attr(..),Modif(..),WMode(..),initYokoPos,initTatePos)
import MyAction (tpsForRelativeLine)
import SDL.Input.Keyboard.Codes

inputEvent :: State -> IO (State,Bool,Bool)
inputEvent st = do
  (kc,md) <- myInput    -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isKeyPressed = kc/=KeycodeUnknown
      isQuit = kc==KeycodeEscape   -- ESC Key
      isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)
      isUp = kc==KeycodeK
      isDown = kc==KeycodeJ
      isLeft = kc==KeycodeH
      isRight = kc==KeycodeL
      texSt = tex st
      atrSt = atr st
      tpsSt = tps st
      tLen = T.length texSt
      wm = wmd atrSt
      tpsPreLine = tpsForRelativeLine atrSt texSt (-1) tpsSt
      tpsNextLine = tpsForRelativeLine atrSt texSt 1 tpsSt
      natr
        | isTglDir = if wm==T then atrSt{gps=initYokoPos,wmd=Y} else atrSt{gps=initTatePos,wmd=T} 
        | otherwise = atrSt
      ntps
        | isUp = if wm==T then if tpsSt==0 then 0 else tpsSt-1 else tpsPreLine
        | isDown = if wm==T then if tpsSt==tLen then tLen else tpsSt+1 else tpsNextLine
        | isLeft = if wm==Y then if tpsSt==0 then 0 else tpsSt-1 else tpsNextLine
        | isRight = if wm==Y then if tpsSt==tLen then tLen else tpsSt+1 else tpsPreLine
        | otherwise = tpsSt
      nst = st{atr=natr,tps=ntps}
  return (nst,isKeyPressed,isQuit)
