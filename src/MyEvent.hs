module MyEvent (inputEvent) where

import MyData (State(..),Attr(..),WMode(..),initYokoPos,initTatePos)
import MySDL.MyInput (myInput)

inputEvent :: State -> IO (State,Bool)
inputEvent st = do
  (kc,md) <- myInput    -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isQuit = kc==27 
      isTglDir = kc==116 && md=='c' -- toggle direction (Tate, Yoko)
      attr = atr st
      wm = wmd attr
      nattr
        | isTglDir = if wm==T then attr{gps=initYokoPos,wmd=Y} else attr{gps=initTatePos,wmd=T} 
        | otherwise = attr
      nst = st{atr=nattr}
  return (nst,isQuit)
