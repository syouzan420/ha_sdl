{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyDraw (myDraw,initDraw) where

import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..),textureAlphaMod
                          ,present,createTextureFromSurface,freeSurface,destroyTexture
                          ,fillRect,drawPoint)
import SDL (($=))
import SDL.Vect (Point(P),V2(..))
import SDL.Font (Font,blended)
import SDL.Primitive (thickLine,rectangle,circle,fillCircle)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (foldM_,when,unless)
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Data.Text (pack)
import General (getIndex)
import MyData (State(..),Attr(..),WMode(..)
              ,IsFormat,Dot,Pos,TextPos,TextData
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..),Color
              ,fontSize,cursorColor,backColor,initTatePos,initYokoPos
              ,dotSize,colorPallet,statusPos,imageNames)

type IsCursor = Bool

myDraw :: Renderer -> [Font] -> [Texture] -> TextData -> Bool -> State -> IO () 
myDraw re fonts itex textData isOnlyMouse st = do
  let (dtsSt,drwSt,imgSt,atrSt,tpsSt,ifmSt,icrSt) =
        (dts st,drw st,img st,atr st,tps st,ifm st,icr st)
      (scrAt,wmdAt) = (scr atrSt,wmd atrSt)
      iniPos = if wmdAt==T then initTatePos else initYokoPos
  initDraw re
  statusDraw re (fonts!!1) st 
  unless isOnlyMouse $ textsDraw re fonts ifmSt icrSt tpsSt textData
  when (tpsSt==0 && icrSt && not ifmSt) $ cursorDraw re (iniPos+scrAt) wmdAt (fromIntegral fontSize) 
  dotsDraw re scrAt dtsSt
  myDrawing re drwSt
  imageDraw re itex imgSt
  present re

imageDraw :: Renderer -> [Texture] -> [Img] -> IO ()
imageDraw re itex = mapM_ (\(Img pos siz rot name) -> 
  when (name `elem` imageNames) $ do
            let ind = getIndex name imageNames
            copyEx re (itex!!ind) (Just (Rectangle (P (V2 0 0)) (V2 64 64)))
                                  (Just (Rectangle (P pos) siz))
                                  (fromIntegral rot) Nothing (V2 False False) ) 

myDrawing :: Renderer -> [Drw] -> IO ()
myDrawing _ [] = return () 
myDrawing re ((Drw cn siz shp):drs) = do
  let col = colorPallet!!cn
  rendererDrawColor re $= col 
  drawShape re col siz shp
  myDrawing re drs

drawShape :: Renderer -> Color -> CInt -> Shp -> IO ()
drawShape re col siz (L (Li ps0 ps1)) = thickLine re ps0 ps1 siz col 
drawShape re col siz (R (Rc False (V2 x y) (V2 w h))) = 
  if w>siz && h>siz then mapM_ (\dp -> rectangle re (V2 (x+dp) (y+dp)) (V2 (x+w-dp) (y+h-dp)) col) [0..(siz-1)] 
                    else fillRect re (Just (Rectangle (P (V2 x y)) (V2 w h)))
drawShape re _ _ (R (Rc True ps wh)) = fillRect re (Just (Rectangle (P ps) wh)) 
drawShape re col siz (C (Cr False ps rd)) = 
  if rd>siz then mapM_ (\dp -> circle re ps (rd-dp) col) [0..(siz-1)] else fillCircle re ps rd col
drawShape re col _ (C (Cr True ps rd)) = fillCircle re ps rd col
drawShape re col siz (D (Dt ps)) =
  if siz==1 then drawPoint re (P ps) else fillCircle re ps (siz-1) col
--drawShape _ _ _ _ = return ()

dotsDraw :: Renderer -> Pos -> [Dot] -> IO () 
dotsDraw re (V2 sx sy) = mapM_ (\(V2 x y,cn) -> do
  let ds = dotSize
  rendererDrawColor re $= colorPallet!!cn 
  fillRect re (Just (Rectangle (P (V2 (x*ds+sx) (y*ds+sy))) (V2 ds ds)))
                    ) 

cursorDraw :: Renderer -> Pos -> WMode -> CInt -> IO () 
cursorDraw re (V2 x y) wm sz = do
  let rect = if wm==T then Rectangle (P (V2 x y)) (V2 sz 2) 
                      else Rectangle (P (V2 (x-1) y)) (V2 2 sz)
  rendererDrawColor re $= cursorColor 
  fillRect re (Just rect) 

statusDraw :: Renderer -> Font -> State -> IO ()
statusDraw re font st = do
  let fileNum = pack$show$fps st
      textPos = pack$show$tps st
      editMode = pack$show$emd st
      scroll = pack$show$scr (atr st)
      fromJump = pack$show$fjp (atr st)
      statusText = "fNum:"<>fileNum<>" tPos:"<>textPos<>" eMode:"<>editMode <>" scr:"<>scroll
                  <>" fjp:"<>fromJump
      ofs = fromIntegral fontSize
      lng = fromIntegral$T.length statusText
  fontS <- blended font (colorPallet!!1) statusText 
  fontT <- createTextureFromSurface re fontS
  mapM_ (\i -> do
     copy re fontT (Just (Rectangle (P (V2 (ofs*i) 0)) (V2 ofs ofs)))
                   (Just (Rectangle (P (statusPos+V2 (12*i) 0)) (V2 12 12)))
         ) [0::CInt,1..lng] 
  destroyTexture fontT
  freeSurface fontS
  

textsDraw :: Renderer -> [Font] -> IsFormat -> IsCursor -> TextPos -> TextData -> IO () 
textsDraw _ _ _ _ _ [] = return () 
textsDraw re fonts ifmSt icrSt tpsSt ((iCur,tx,nat,pList):xs) = do
  let (scrAt,wmdAt,fszAt,fcoAt,iosAt) = (scr nat,wmd nat,fsz nat,fco nat,ios nat)
      ofs = fromIntegral fontSize
      fs = fromIntegral fszAt
      fnum = if iosAt then 2 else 1
      nscr = if null xs then scrAt else let (_,_,nxtAtr,_) = head xs in scr nxtAtr
      rpText = T.replace "\n" "  " tx
      lPos = snd$last pList
  when (tx/=T.empty) $ do
        fontS <- case fnum of
                 1 -> blended (fonts!!fnum) fcoAt tx 
                 2 -> blended (fonts!!fnum) fcoAt rpText
                 _ -> blended (fonts!!1) fcoAt tx
        fontT <- createTextureFromSurface re fontS
        foldM_ (\ ps ((b,r),pd) -> do
          let sz = if b then ofs `div` 2 else ofs
          copyEx re fontT (Just (Rectangle (P ps) (V2 sz ofs)))
                          (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
                          (if wmdAt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 sz 0)
              ) (V2 0 0) pList
        destroyTexture fontT
        freeSurface fontS
  when (iCur && icrSt && not ifmSt) $ cursorDraw re (lPos+nscr) wmdAt fs 
  textsDraw re fonts ifmSt icrSt tpsSt xs

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re


