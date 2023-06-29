{-# LANGUAGE OverloadedStrings #-}
module MyAction (myAction,beforeDraw,afterDraw,makePList,tpsForRelativeLine,changeAtr) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import SDL.Vect (Point(P),V2(..),V4(..))
import MyData (State(..),Attr(..),WMode(..),Pos)

type Index = Int
type Line = Int
type Letter = Int
type Location = (Line,Letter)

myAction :: State -> State
myAction st = st

beforeDraw :: State -> State 
beforeDraw st = 
  let crcSt = crc st
      icrSt = icr st
      ncrc = if crcSt<10 then crcSt+1 else 0
      nicr = if crcSt<10 then icrSt else not icrSt
   in st{crc=ncrc, icr=nicr}

afterDraw :: State -> State
afterDraw st = st

tpsForRelativeLine :: Attr -> Text -> Int -> Index -> Index 
tpsForRelativeLine atrSt texSt rdv ind =
  let (ln,lt) = indexToLoc atrSt texSt ind   
      nind = locToIndex atrSt texSt (ln+rdv,lt)
   in if nind<0 then ind else nind

indexToLoc :: Attr -> Text -> Index -> Location
indexToLoc atrSt texSt ind = indexToLoc' atrSt (T.take ind texSt) (0,0)

indexToLoc' :: Attr -> Text -> Location -> Location
indexToLoc' attr@(Attr ps wm _ _ tw nw ws@(V2 _ wh) mg@(V4 _ _ _ mb) _) tx lc =
  case uncons tx of
    Nothing -> lc 
    Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch xs tw nw wm ps ws mg lc 
                     in indexToLoc' attr{gps=npos} xs (nln,nlt)

locToIndex :: Attr -> Text -> Location -> Index
locToIndex atrSt texSt tlc = locToIndex' atrSt texSt tlc (0,0) 0 

locToIndex' :: Attr -> Text -> Location -> Location -> Index -> Index
locToIndex' attr@(Attr ps wm _ _ tw nw ws@(V2 _ wh) mg@(V4 _ _ _ mb) _) tx tlc@(tln,tlt) lc@(ln,lt) ind
  | lc==tlc = ind 
  | ln>tln && tlt > lt = ind-1 
  | otherwise =
      case uncons tx of
        Nothing -> if tlt>lt then ind else (-1) 
        Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch xs tw nw wm ps ws mg lc 
                         in locToIndex' attr{gps=npos} xs tlc (nln,nlt) (ind+1)


makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList atrSt@(Attr ps@(V2 ox oy) wm _ _ tw nw ws mg _) tx = 
  case uncons tx of
    Nothing -> [((False,False),ps)]
    Just (ch,xs) -> let ((ihf,irt),(npos,_)) = nextPos ch xs tw nw wm ps ws mg (0,0) 
                        qtw = tw `div` 4
                        ihft = wm==T && ihf
                     in ((ihf,irt),V2 (if ihft then ox+qtw else ox) (if ihft then oy-qtw else oy))
                          :makePList atrSt{gps=npos} xs

nextPos :: Char -> Text -> CInt -> CInt -> WMode -> Pos -> V2 CInt -> V4 CInt -> Location 
                                                            -> ((Bool,Bool),(Pos,Location))
nextPos ch xs tw nw wm ps@(V2 ox oy) (V2 ww wh) (V4 mr mt ml mb) (ln,lt) = 
    let cn = fromEnum ch
        htw = tw `div` 2
        ihf = cn > 31 && cn < 127
        irt = ch `T.elem` "＝ー「」（）：；"
        inl = ch == '\n'
        ins = ch `T.elem` "\n"
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
     in ((ihf,irt),(npos,(nln,nlt)))

changeAtr :: Attr -> Text -> (Attr, (Text, Text))
changeAtr attr@(Attr gpsAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt rbiAt) tx = 
  let (com,rtx) = T.break (==' ') tx
      natr = case com of
               "rb" -> attr
               _    -> attr
   in (attr , (tx, T.empty))


