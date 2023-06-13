module MySDL.MyDraw (myDraw,initDraw) where

import SDL.Video (Renderer, Texture)
import SDL.Video.Renderer (rendererDrawColor,clear,copy,copyEx,Rectangle(..),textureAlphaMod
                          ,present,createTextureFromSurface,freeSurface)
import SDL (($=))
import SDL.Vect (Point(P),V2(..),V4(..))
import SDL.Font (Font,blended)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad (foldM_)
import Foreign.C.Types (CInt)
import qualified Data.Text as T
import Data.Text (Text,uncons)
import MyData (State(..),Attr(..),WMode(..),Pos,Color,fontSize,backColor)

myDraw :: Renderer -> [Font] -> [Texture] -> State -> IO () 
myDraw re fonts itexs (State texSt tpsSt atrSt) = do
  initDraw re
  textsDraw re fonts atrSt texSt 
  present re

textsDraw :: Renderer -> [Font] -> Attr -> Text -> IO ()
textsDraw re fonts atrSt texSt = do
  case uncons texSt of
    Nothing -> return ()
    Just (ch,tailTx) -> do 
      let (natr,(tx,xs)) = if ch=='\"' then changeAtr atrSt tailTx else (atrSt,T.break (=='\"') texSt)
          (Attr gpsAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt) = natr
          ofs = fromIntegral fontSize
          fs = fromIntegral fszAt
          pList = makePList natr tx
      fontS <- blended (fonts!!1) fcoAt tx 
      fontT <- createTextureFromSurface re fontS
      freeSurface fontS
      foldM_ (\ ps ((b,r),pd) -> do
        let sz = if b then ofs `div` 2 else ofs
        copyEx re fontT (Just (Rectangle (P ps) (V2 sz ofs)))
                        (Just (Rectangle (P pd) (V2 (if b then fs `div` 2 else fs) fs)))
                        (if wmdAt==T && (b||r) then 90 else 0) Nothing (V2 False False)
        return (ps+V2 sz 0)
             ) (V2 0 0) pList
      textsDraw re fonts (Attr (snd$last pList) wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt) xs

makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList attr@(Attr ps@(V2 ox oy) wm fs fc tw nw (V2 ww wh) (V4 mr mt ml mb)) tx = 
  case uncons tx of
    Nothing -> []
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

changeAtr :: Attr -> Text -> (Attr, (Text, Text))
changeAtr ast tx = (ast , (tx, T.empty))

initDraw :: MonadIO m => Renderer -> m ()
initDraw re = do
  rendererDrawColor re $= backColor 
  clear re
