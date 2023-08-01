{-# LANGUAGE OverloadedStrings #-}
module MySDL.MyDraw (myDraw,initDraw) where

import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..),textureAlphaMod
                          ,present,createTextureFromSurface,freeSurface,fillRect)
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Font (Font,blended)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (foldM_,when)
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import qualified Data.Text.IO as TI
import Data.Text (Text,uncons,pack)
import MyData (Dots,State(..),Attr(..),Rubi(..),WMode(..),Pos,Color,PList,fontSize,cursorColor,backColor,initTatePos,initYokoPos,dotSize,colorPallet,statusPos)
import MyAction (makePList,changeAtr,exeAttrCom)

type Index = Int
type IsFormat = Bool
type IsCursor = Bool
type TextPos = Int
type Line = Int
type Letter = Int
type Location = (Line,Letter)
type TextData = [(Bool,Text,Attr,[PList])]

myDraw :: Renderer -> [Font] -> [Texture] -> TextData -> State -> IO () 
myDraw re fonts itexs textData st@(State texSt dtsSt atrSt _ tpsSt _ _ _ ifmSt icrSt) = do
  initDraw re
  statusDraw re (fonts!!1) st 
  textsDraw re fonts ifmSt icrSt tpsSt textData 
  let scrAt = scr atrSt
  dotsDraw re scrAt dtsSt
  present re

dotsDraw :: Renderer -> Pos -> Dots -> IO () 
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
      position = pack$show$gps (atr st)
      scroll = pack$show$scr (atr st)
      statusText = "fNum:"<>fileNum<>" tPos:"<>textPos<>" eMode:"<>editMode<>" gPos:"<>position
                <>" scr:"<>scroll
      ofs = fromIntegral fontSize
      lng = fromIntegral$T.length statusText
  fontS <- blended font (colorPallet!!1) statusText 
  fontT <- createTextureFromSurface re fontS
  mapM_ (\i -> do
     copy re fontT (Just (Rectangle (P (V2 (ofs*i) 0)) (V2 ofs ofs)))
                   (Just (Rectangle (P (statusPos+V2 (12*i) 0)) (V2 12 12)))
         ) [0::CInt,1..lng] 
  

textsDraw :: Renderer -> [Font] -> IsFormat -> IsCursor -> TextPos -> [(Bool,Text,Attr,[PList])] -> IO () 
textsDraw _ _ _ _ _ [] = return ()
textsDraw re fonts ifmSt icrSt tpsSt ((iCur,tx,natr,pList):xs) = do
  let (Attr gpsAt scrAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt rbiAt cnmAt cidAt iosAt) = natr
      ofs = fromIntegral fontSize
      fs = fromIntegral fszAt
      fnum = if iosAt then 2 else 1
      nscr = if null xs then scrAt else let (_,_,nxtAtr,_) = head xs in scr nxtAtr
      rpText = T.replace "\n" "  " tx
      lPos = snd$last pList
      iniPos = if wmdAt==T then initTatePos else initYokoPos
  when (tx/=T.empty) $ do
        fontS <- case fnum of
                 1 -> blended (fonts!!fnum) fcoAt tx 
                 2 -> blended (fonts!!fnum) fcoAt rpText
                 _ -> blended (fonts!!1) fcoAt tx
        fontT <- createTextureFromSurface re fontS
--      freeSurface fontS
        when (tpsSt==0 && icrSt && not ifmSt) $ cursorDraw re (iniPos+scrAt) wmdAt fs
        foldM_ (\ ps ((b,r),pd) -> do
          let sz = if b then ofs `div` 2 else ofs
          copyEx re fontT (Just (Rectangle (P ps) (V2 sz ofs)))
                          (Just (Rectangle (P (pd+nscr)) (V2 (if b then fs `div` 2 else fs) fs)))
                          (if wmdAt==T && (b||r) then 90 else 0) Nothing (V2 False False)
          return (ps+V2 sz 0)
              ) (V2 0 0) pList
  when (iCur && icrSt && not ifmSt) $ cursorDraw re (lPos+nscr) wmdAt fs 
  textsDraw re fonts ifmSt icrSt tpsSt xs

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re


