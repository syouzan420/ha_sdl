{-# LANGUAGE OverloadedStrings #-}
module MyEvent (inputEvent) where

import MySDL.MyInput (myInput)
import Linear.V2 (V2(..))
import qualified Data.Text as T
import Data.List (nub)
import MyData (Pos,Dot,Dots,State(..),Attr(..),Modif(..),WMode(..),EMode(..),initYokoPos,initTatePos,dotSize,colorPallet)
import MyAction (tpsForRelativeLine)
import SDL.Input.Keyboard.Codes

inputEvent :: State -> IO (State,Bool,Bool,Bool,Bool,Bool)
inputEvent st@(State texSt dtsSt atrSt _ tpsSt _ emdSt cplSt ifmSt _) = do
  (kc,md,it,mps,isc) <- myInput    -- md: keyModifier ('a'-alt, 'c'-control, 's'-shift, ' '-nothing)
  let isKeyPressed = kc/=KeycodeUnknown
      isMousePressed = mps/=V2 (-1) (-1)
      isQuit = kc==KeycodeEscape   -- ESC Key
      isNewFile = kc==KeycodeN && md==Ctr
      isLoadFile = kc==KeycodeL && md==Ctr
      isTglDir = kc==KeycodeT && md==Ctr -- toggle direction (Tate, Yoko)
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
      isCom = md==Alt
      isDrawClear = kc==KeycodeD && md==Ctr
      isTglColor = kc==KeycodeC && md==Ctr

      comName = case kc of
                  KeycodeR -> ";rb "
                  _        -> T.empty
      tLen = T.length texSt
      wm = wmd atrSt
      os = ios atrSt
      scrAt = scr atrSt
      ncpl = if isTglColor then if cplSt==length colorPallet - 1 then 0 else cplSt+1 else cplSt
      tpsPreLine = tpsForRelativeLine atrSt texSt (-1) tpsSt
      tpsNextLine = tpsForRelativeLine atrSt texSt 1 tpsSt
      nit = if isIns && isRet then "\n" else it
      textIns tx = T.take tpsSt texSt <> tx <> T.drop tpsSt texSt 
      natr
        | isTglDir = if wm==T then atrSt{gps=initYokoPos,wmd=Y,scr=V2 0 0} 
                              else atrSt{gps=initTatePos,wmd=T,scr=V2 0 0} 
        | isTglOsd = if os then atrSt{ios=False} else atrSt{ios=True}
        | otherwise = atrSt
      ntps
        | ifmSt = tpsSt
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
      nst = st{tex=ntex,dts=ndts,atr=natr,tps=ntps,emd=nemd,cpl=ncpl,ifm=nifm}
  return (nst,isKeyPressed,isMousePressed,isNewFile,isLoadFile,isQuit)

toDotPos :: Pos -> Pos -> Pos
toDotPos (V2 px py) (V2 sx sy) = let ds = dotSize
                                     nx = (px-sx) `div` ds; ny = (py-sy) `div` ds
                                  in V2 nx ny

addMidDots :: Dot -> Dot -> Dots 
addMidDots (V2 x0 y0,cn) (V2 x1 y1,_) 
  | x0==x1 && y0==y1 = []
  | x0==x1 = map (\doty -> (V2 x0 doty,cn)) (if y1>y0 then [y0..y1] else [y1..y0])
  | y0==y1 = map (\dotx -> (V2 dotx y0,cn)) (if x1>x0 then [x0..x1] else [x1..x0])
  | otherwise = nub$map (\dotx -> (V2 dotx (f dotx),cn)) (if x1>x0 then [x0..x1] else [x1..x0]) ++
                    map (\doty -> (V2 (g doty) doty,cn)) (if y1>y0 then [y0..y1] else [y1..y0])
     where f x = floor$(fromIntegral (y1-y0) ::Double)/fromIntegral (x1-x0)*(fromIntegral x-fromIntegral x0) + fromIntegral y0
           g y = floor$(fromIntegral (x1-x0) ::Double)/fromIntegral (y1-y0)*(fromIntegral y-fromIntegral y0) + fromIntegral x0
