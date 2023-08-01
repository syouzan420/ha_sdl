{-# LANGUAGE OverloadedStrings #-}
module MyAction (myAction,beforeDraw,afterDraw,makePList,tpsForRelativeLine
                ,changeAtr,exeAttrCom,makeTextData,dotsToRect) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import SDL.Vect (Point(P),V2(..),V4(..))
import MyData (Dots,State(..),Attr(..),Rubi(..),WMode(..),PList,Pos,rubiSize,dotSize)

type Index = Int
type Line = Int
type Letter = Int
type TextPos = Int
type Location = (Line,Letter)
type IsFormat = Bool
type Rect = V4 CInt

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

makeTextData :: State -> [(Bool,Text,Attr,[PList])]
makeTextData (State texSt _ atrSt _ tpsSt _ _ _ ifmSt _) = makeTexts 0 ifmSt tpsSt atrSt texSt

makeTexts :: Index -> IsFormat -> TextPos -> Attr -> Text -> [(Bool,Text,Attr,[PList])]
makeTexts ind ifmSt tpsSt atrSt texSt = 
  case uncons texSt of
    Nothing -> [] 
    Just (ch,tailTx) ->  
      let (natr,(ptx,pxs)) 
            | ifmSt = if ch==';' then exeAttrCom$changeAtr atrSt tailTx else
                        if cnm atrSt/=T.empty then exeAttrCom (atrSt,texSt)
                                              else (atrSt,T.break (==';') texSt)
            | otherwise = (atrSt,(texSt,T.empty))
          lnTex = T.length texSt 
          preInc = lnTex - T.length pxs + 1
          iCur = tpsSt > ind && tpsSt < ind + preInc && not ifmSt
          (iptx,tptx) = if iCur && tpsSt>0 then T.splitAt (tpsSt-ind) ptx else (ptx,T.empty) 
          (tx,xs) = if iCur then (iptx,tptx<>pxs) else (ptx,pxs)
          (Attr gpsAt scrAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt rbiAt cnmAt cidAt iosAt) = natr
          fs = fromIntegral fszAt
          pList = makePList natr tx
          indInc = lnTex - T.length xs + 1
          lPos@(V2 lpx lpy) = snd$last pList
          (V2 sx sy) = scrAt
          (V2 ww wh) = wszAt
          (V4 mr mt ml mb) = mgnAt
          nscr
            | iCur && wmdAt == T && lpx+sx < ml = V2 (ml-lpx) sy 
            | iCur && wmdAt == T && lpx+sx > ww - mr - fs  = V2 (ww-mr-fs*2-lpx) sy
            | iCur && wmdAt == Y && lpy+sy > wh - mb - fs = V2 sx (wh-mb-fs-lpy)
            | iCur && wmdAt == Y && lpy+sy < mt = V2 sx (mt+fs-lpy)
            | otherwise = scrAt
      in (iCur,tx,natr{gps=lPos,scr=nscr},pList):makeTexts (ind+indInc) ifmSt tpsSt natr{gps=lPos,scr=nscr} xs 



tpsForRelativeLine :: Attr -> Text -> Int -> Index -> Index 
tpsForRelativeLine atrSt texSt rdv ind =
  let (ln,lt) = indexToLoc atrSt texSt ind   
      nind = locToIndex atrSt texSt (ln+rdv,lt)
   in if nind<0 then ind else nind

indexToLoc :: Attr -> Text -> Index -> Location
indexToLoc atrSt texSt ind = indexToLoc' atrSt (T.take ind texSt) (0,0)

indexToLoc' :: Attr -> Text -> Location -> Location
indexToLoc' attr@(Attr ps _ wm _ _ tw nw ws@(V2 _ wh) mg@(V4 _ _ _ mb) _ _ _ _) tx lc =
  case uncons tx of
    Nothing -> lc 
    Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch xs tw nw wm ps ws mg lc 
                     in indexToLoc' attr{gps=npos} xs (nln,nlt)

locToIndex :: Attr -> Text -> Location -> Index
locToIndex atrSt texSt tlc = locToIndex' atrSt texSt tlc (0,0) 0 

locToIndex' :: Attr -> Text -> Location -> Location -> Index -> Index
locToIndex' attr@(Attr ps _ wm _ _ tw nw ws@(V2 _ wh) mg@(V4 _ _ _ mb) _ _ _ _) tx tlc@(tln,tlt) lc@(ln,lt) ind
  | lc==tlc = ind 
  | ln>tln && tlt > lt = ind-1 
  | otherwise =
      case uncons tx of
        Nothing -> if tlt>lt then ind else (-1) 
        Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch xs tw nw wm ps ws mg lc 
                         in locToIndex' attr{gps=npos} xs tlc (nln,nlt) (ind+1)


makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList atrSt@(Attr ps@(V2 ox oy) _ wm _ _ tw nw ws mg _ _ _ _) tx = 
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

changeAtr :: Attr -> Text -> (Attr, Text)
changeAtr attr tx = 
  let (com,rtx) = T.break (==' ') tx
      ncid = case com of
               "rb" -> 2 
               _    -> 0
      natr = attr{cnm=com, cid=ncid}
   in (natr , rtx)

exeAttrCom :: (Attr,Text) -> (Attr, (Text, Text))
exeAttrCom (attr@(Attr gpsAt scrAt wmdAt fszAt fcoAt ltwAt lnwAt wszAt mgnAt rbiAt cnmAt cidAt iosAt),tx) = 
  let (Rubi rpsRb rwdRb tszRb tlwRb sprRb) = rbiAt
      tailTx = T.tail tx
      (ttx,rtx) = if cidAt>0 then breakText tailTx  else T.break (==';') tailTx
      tln = fromIntegral (T.length ttx)
      natr = case cnmAt of
               "rb" -> case cidAt of
                         2 -> attr{rbi=rbiAt{rps=gpsAt,rwd=ltwAt*tln}}
                         1 -> let fs = fromIntegral fszAt
                                  rbStartPos = if wmdAt==T then rpsRb + V2 (fs+sprRb) 0  
                                                           else rpsRb - V2 0 (fromIntegral rubiSize+sprRb)
                                  rbLetterWidth = rwdRb `div` tln 
                               in attr{gps=rbStartPos,fsz=rubiSize,ltw=rbLetterWidth 
                                      ,rbi=rbiAt{tsz=fszAt,tlw=ltwAt}} 
                         0 -> attr{gps=rpsRb+(if wmdAt==T then V2 0 rwdRb else V2 rwdRb 0)
                                  ,fsz=tszRb, ltw=tlwRb}
               _    -> attr{cnm=""}
      ncnm = if cidAt==0 then "" else cnmAt
   in (natr{cnm=ncnm, cid=cidAt-1} , (ttx, rtx))

breakText :: Text -> (Text,Text)
breakText tx = let (hd,tl) = T.break (=='\n') tx
                   (hda,hdb) = if ' ' `T.elem` hd then T.break (==' ') hd else (hd,T.empty)
                   tl2
                     |tl==T.empty = tl 
                     |T.head tl == '\n' = " "<>tl
                     |otherwise = tl
                in (hda,hdb<>tl2)

dotsToRect :: Dots -> [Rect]
dotsToRect dtl = let ds = dotSize
                 in map (\(V2 x y,_) -> V4 (x*ds) (y*ds) ds ds) dtl 

