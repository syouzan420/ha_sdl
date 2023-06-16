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
import Data.Text (Text,uncons)
import MyData (State(..),Attr(..),WMode(..),Pos,Color,fontSize,cursorColor,backColor)

type Index = Int
type IsFormat = Bool
type IsCursor = Bool
type TextPos = Int
type Line = Int
type Letter = Int
type Location = (Line,Letter)

myDraw :: Renderer -> [Font] -> [Texture] -> State -> IO () 
myDraw re fonts itexs (State texSt atrSt tpsSt _ ifmSt icrSt) = do
  initDraw re
  textsDraw re fonts 0 ifmSt icrSt tpsSt atrSt texSt 
  present re

cursorDraw :: Renderer -> Pos -> WMode -> CInt -> IO () 
cursorDraw re (V2 x y) wm sz = do
  let rect = if wm==T then Rectangle (P (V2 x y)) (V2 sz 2) 
                      else Rectangle (P (V2 (x-1) y)) (V2 2 sz)
  rendererDrawColor re $= cursorColor 
  fillRect re (Just rect) 

textsDraw :: Renderer -> [Font] -> Index -> IsFormat
                      -> IsCursor -> TextPos -> Attr -> Text -> IO ()
textsDraw re fonts ind ifmSt icrSt tpsSt atrSt texSt = do
  case uncons texSt of
    Nothing -> return ()
    Just (ch,tailTx) -> do 
      let (natr,(ptx,pxs)) 
            | ifmSt = if ch=='\"' then changeAtr atrSt tailTx else (atrSt,T.break (=='\"') texSt)
            | otherwise = (atrSt,(texSt,T.empty))
          lnTex = T.length texSt 
          preInc = lnTex - T.length pxs + 1
          iCur = tpsSt > ind && tpsSt < ind + preInc
          (iptx,tptx) = if iCur && tpsSt>0 then T.splitAt (tpsSt-ind) ptx else (ptx,T.empty) 
          (tx,xs) = if iCur then (iptx,tptx<>pxs) else (ptx,pxs)
          (Attr gpsAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt) = natr
          ofs = fromIntegral fontSize
          fs = fromIntegral fszAt
          pList = makePList natr tx
          indInc = lnTex - T.length xs + 1
          lPos = snd$last pList
      fontS <- blended (fonts!!1) fcoAt tx 
      fontT <- createTextureFromSurface re fontS
--      freeSurface fontS
      when (tpsSt==0 && icrSt) $ cursorDraw re gpsAt wmdAt fs
      foldM_ (\ ps ((b,r),pd) -> do
        let sz = if b then ofs `div` 2 else ofs
        copyEx re fontT (Just (Rectangle (P ps) (V2 sz ofs)))
                        (Just (Rectangle (P pd) (V2 (if b then fs `div` 2 else fs) fs)))
                        (if wmdAt==T && (b||r) then 90 else 0) Nothing (V2 False False)
        return (ps+V2 sz 0)
             ) (V2 0 0) pList
      when (iCur && icrSt) $ cursorDraw re lPos wmdAt fs 
      textsDraw re fonts (ind+indInc) ifmSt icrSt tpsSt natr{gps=lPos} xs

indexToLoc :: Attr -> Text -> Index -> Location
indexToLoc atrSt texSt ind = indexToLoc' atrSt (T.take ind texSt) (0,0)

indexToLoc' :: Attr -> Text -> Location -> Location
indexToLoc' attr@(Attr ps@(V2 ox oy) wm fs fc tw nw (V2 ww wh) (V4 mr mt ml mb)) tx (ln,lt) =
  case uncons tx of
    Nothing -> (ln,lt) 
    Just (ch,xs) -> let cn = fromEnum ch
                        htw = tw `div` 2
                        qtw = htw `div` 2
                        ihf = cn > 31 && cn < 127
                        irt = ch `elem` "＝ー「」（）"
                        inl = ch == '\n'
                        ins = ch `elem` "\n"
                        ihft = wm==T && ihf
                        delta 
                          | wm==T = if ihf then V2 0 htw else V2 0 tw
                          | ihf = V2 htw 0 
                          | otherwise = V2 tw 0
                        psd@(V2 nx ny) = if ins then ps else ps + delta 
                        npos
                          | wm==T = if ny > wh - mb || inl then V2 ox mt - V2 nw 0 else psd 
                          | nx  > ww - mr || inl = V2 ml oy + V2 0 nw
                          | otherwise = psd
                        (nln,nlt)
                          | ny > wh - mb || inl = (ln+1,0)
                          | otherwise = (ln,lt+1)
                     in indexToLoc' attr{gps=npos} xs (nln,nlt)


makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList atrSt@(Attr ps@(V2 ox oy) wm _ _ tw nw ws mg) tx = 
  case uncons tx of
    Nothing -> [((False,False),ps)]
    Just (ch,xs) -> let ((ihf,irt),npos) = nextPos ch xs tw nw wm ps ws mg 
                        qtw = tw `div` 4
                        ihft = wm==T && ihf
                     in ((ihf,irt),V2 (if ihft then ox+qtw else ox) (if ihft then oy-qtw else oy))
                          :makePList atrSt{gps=npos} xs

nextPos :: Char -> Text -> CInt -> CInt -> WMode -> Pos -> V2 CInt -> V4 CInt -> ((Bool,Bool),Pos)
nextPos ch xs tw nw wm ps@(V2 ox oy) (V2 ww wh) (V4 mr mt ml mb) = 
    let cn = fromEnum ch
        htw = tw `div` 2
        ihf = cn > 31 && cn < 127
        irt = ch `elem` "＝ー「」（）"
        inl = ch == '\n'
        ins = ch `elem` "\n"
        delta 
           | wm==T = if ihf then V2 0 htw else V2 0 tw
           | ihf = V2 htw 0 
           | otherwise = V2 tw 0
        psd@(V2 nx ny) = if ins then ps else ps + delta 
        npos
           | wm==T = if ny > wh - mb || inl then V2 ox mt - V2 nw 0 else psd 
           | nx  > ww - mr || inl = V2 ml oy + V2 0 nw
           | otherwise = psd
     in ((ihf,irt),npos)
       {--
makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList attr@(Attr ps@(V2 ox oy) wm fs fc tw nw (V2 ww wh) (V4 mr mt ml mb)) tx = 
  case uncons tx of
    Nothing -> [((False,False),ps)]
    Just (ch,xs) -> let cn = fromEnum ch
                        htw = tw `div` 2
                        qtw = htw `div` 2
                        ihf = cn > 31 && cn < 127
                        irt = ch `elem` "＝ー「」（）"
                        inl = ch == '\n'
                        ins = ch `elem` "\n"
                        ihft = wm==T && ihf
                        delta 
                          | wm==T = if ihf then V2 0 htw else V2 0 tw
                          | ihf = V2 htw 0 
                          | otherwise = V2 tw 0
                        psd@(V2 nx ny) = if ins then ps else ps + delta 
                        npos
                          | wm==T = if ny > wh - mb || inl then V2 ox mt - V2 nw 0 else psd 
                          | nx  > ww - mr || inl = V2 ml oy + V2 0 nw
                          | otherwise = psd
                     in ((ihf,irt),V2 (if ihft then ox+qtw else ox) (if ihft then oy-qtw else oy))
                          :makePList attr{gps=npos} xs

--}

changeAtr :: Attr -> Text -> (Attr, (Text, Text))
changeAtr ast tx = (ast , (tx, T.empty))

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re
