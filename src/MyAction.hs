{-# LANGUAGE OverloadedStrings #-}
module MyAction (myAction,beforeDraw,afterDraw,makePList,tpsForRelativeLine
                ,changeAtr,exeAttrCom,makeTextData,dotsToRect,locToIndex) where

import Data.Text (Text,uncons)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import SDL.Vect (V2(..),V4(..))
import Data.Maybe(fromMaybe)
import Data.List(elemIndex)
import MyData (IsFormat,Pos,TextPos,TextData,Dot,Jump,FrJp,State(..),Attr(..),Rubi(..),WMode(..)
              ,rubiSize,dotSize,textLengthLimit,linkColor,selectColor,fontColor,cursorTime)

type Index = Int
type Line = Int
type Letter = Int
type FilePos = Int
type Location = (Line,Letter)
type Rect = V4 CInt

myAction :: State -> State
myAction st = st

beforeDraw :: State -> State 
beforeDraw st = 
  let crcSt = crc st
      icrSt = icr st
      ncrc = if crcSt<cursorTime then crcSt+1 else 0
      nicr = if crcSt<cursorTime then icrSt else not icrSt
   in st{crc=ncrc, icr=nicr}

afterDraw :: State -> State
afterDraw st = st

makeTextData :: State -> TextData 
makeTextData (State texSt _ atrSt fpsSt tpsSt _ _ _ ifmSt _ _ _) =
  makeTexts 0 ifmSt fpsSt tpsSt atrSt texSt

makeTexts :: Index -> IsFormat -> FilePos -> TextPos -> Attr -> Text -> TextData 
makeTexts ind ifmSt fpsSt tpsSt atrSt texSt = 
  case uncons texSt of
    Nothing -> [] 
    Just (ch,tailTx) ->  
      let (natr,(ptx,pxs)) 
            | ifmSt = if ch==';' then exeAttrCom fpsSt ind (changeAtr atrSt{ite=False} tailTx) else
                        if cnm atrSt/=T.empty then exeAttrCom fpsSt ind (atrSt{ite=False},texSt)
                                              else (atrSt,T.break (==';') texSt)
            | otherwise = (atrSt,(texSt,T.empty))
          tll = textLengthLimit
          (ptx2,pxs2) = if T.length ptx>tll then (T.take tll ptx,T.drop tll ptx<>pxs) else (ptx,pxs)
          lnTex = T.length texSt 
          preInc = lnTex - T.length pxs2 + 1
          iCur = tpsSt > ind && tpsSt < ind + preInc && not ifmSt
          (iptx,tptx) = if iCur && tpsSt>0 then T.splitAt (tpsSt-ind) ptx2 else (ptx2,T.empty) 
          (tx,xs) = if iCur then (iptx,tptx<>pxs2) else (ptx2,pxs2)
          (scrAt, wmdAt, fszAt, wszAt, mgnAt) = (scr natr,wmd natr,fsz natr,wsz natr,mgn natr)
          fs = fromIntegral fszAt
          pList = makePList natr tx
          indInc = lnTex - T.length xs 
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
      in (iCur,tx,natr{gps=lPos,scr=nscr},pList):makeTexts (ind+indInc) ifmSt fpsSt tpsSt natr{gps=lPos,scr=nscr} xs 

tpsForRelativeLine :: Attr -> Text -> Int -> Index -> Index 
tpsForRelativeLine atrSt texSt rdv ind =
  let (ln,lt) = indexToLoc atrSt texSt ind   
      nind = locToIndex atrSt texSt (ln+rdv,lt)
   in if nind<0 then ind else nind

indexToLoc :: Attr -> Text -> Index -> Location
indexToLoc atrSt texSt ind = indexToLoc' atrSt (T.take ind texSt) (0,0)

indexToLoc' :: Attr -> Text -> Location -> Location
indexToLoc' at tx lc =
  let (ps,wm,tw,nw,ws,mg) = (gps at,wmd at,ltw at,lnw at,wsz at,mgn at)
   in case uncons tx of
    Nothing -> lc 
    Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch tw nw wm ps ws mg lc 
                     in indexToLoc' at{gps=npos} xs (nln,nlt)

locToIndex :: Attr -> Text -> Location -> Index
locToIndex atrSt texSt tlc = locToIndex' atrSt texSt tlc (0,0) 0 

locToIndex' :: Attr -> Text -> Location -> Location -> Index -> Index
locToIndex' at tx tlc@(tln,tlt) lc@(ln,lt) ind
  | lc==tlc = ind 
  | ln>tln && tlt > lt = ind-1 
  | otherwise =
      case uncons tx of
        Nothing -> if tlt>lt || tln>ln then ind else (-1) 
        Just (ch,xs) -> let (_,(npos,(nln,nlt))) = nextPos ch tw nw wm ps ws mg lc 
                         in locToIndex' at{gps=npos} xs tlc (nln,nlt) (ind+1)
  where (ps,wm,tw,nw,ws,mg) = (gps at,wmd at,ltw at,lnw at,wsz at,mgn at)


makePList :: Attr -> Text -> [((Bool,Bool),V2 CInt)]
makePList at tx = 
  let (ps@(V2 ox oy),wm,tw,nw,ws,mg) = (gps at,wmd at,ltw at,lnw at,wsz at,mgn at)
   in case uncons tx of
    Nothing -> [((False,False),ps)]
    Just (ch,xs) -> let ((ihf,irt),(npos,_)) = nextPos ch tw nw wm ps ws mg (0,0) 
                        qtw = tw `div` 4
                        ihft = wm==T && ihf
                     in ((ihf,irt),V2 (if ihft then ox+qtw else ox) (if ihft then oy-qtw else oy))
                          :makePList at{gps=npos} xs

nextPos :: Char -> CInt -> CInt -> WMode -> Pos -> V2 CInt -> V4 CInt -> Location 
                                                            -> ((Bool,Bool),(Pos,Location))
nextPos ch tw nw wm ps@(V2 ox oy) (V2 ww wh) (V4 mr mt ml mb) (ln,lt) = 
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
           | wm==T && (ny > wh - mb || inl) = (ln+1,0)
           | wm==Y && (nx > ww - mr || inl) = (ln+1,0)
           | otherwise = (ln,lt+1)
     in ((ihf,irt),(npos,(nln,nlt)))

changeAtr :: Attr -> Text -> (Attr, Text)
changeAtr attr tx = 
  let (com,rtx) = T.break (==' ') tx
      ncid = case com of
               "rb" -> 2 
               "jtg" -> 1
               "jp" -> 3
               _    -> 0
      natr = attr{cnm=com, cid=ncid}
   in (natr , rtx)

exeAttrCom :: FilePos -> TextPos -> (Attr,Text) -> (Attr, (Text, Text))
exeAttrCom fpsSt tpsSt 
    (at@(Attr gpsAt _ wmdAt fszAt _ ltwAt _ _ _ dtaAt rbiAt jpsAt fjpAt sjnAt cnmAt cidAt _ _),tx) = 
  let (Rubi rpsRb rwdRb tszRb tlwRb sprRb) = rbiAt
      tailTx = T.tail tx
      (ttx,rtx) = if cidAt>0 then breakText tailTx  else T.break (==';') tailTx
      tln = fromIntegral (T.length ttx)
      natr = case cnmAt of
               "rb" -> case cidAt of
                         2 -> at{rbi=rbiAt{rps=gpsAt,rwd=ltwAt*tln}}
                         1 -> let fs = fromIntegral fszAt
                                  rbStartPos = if wmdAt==T then rpsRb + V2 (fs+sprRb) 0  
                                                           else rpsRb - V2 0 (fromIntegral rubiSize+sprRb)
                                  rbLetterWidth = rwdRb `div` tln 
                               in at{gps=rbStartPos,fsz=rubiSize,ltw=rbLetterWidth 
                                    ,rbi=rbiAt{tsz=fszAt,tlw=ltwAt}} 
                         0 -> at{gps=rpsRb+(if wmdAt==T then V2 0 rwdRb else V2 rwdRb 0)
                                  ,fsz=tszRb, ltw=tlwRb}
                         _ -> at
               "jtg" -> case cidAt of
                          1 -> let jd = textToJumpData fpsSt tpsSt ttx
                                   dataExist = jd `elem` jpsAt
                                   njps = if dataExist then jpsAt else jpsAt ++ [jd]
                                in at{jps=njps,ite=True}
                          _ -> at
               "jp" -> case cidAt of
                         3 -> at{dta=[ttx],ite=True} 
                         2 -> at{dta=dtaAt++[ttx],ite=True} 
                         1 -> let tjp = searchJump jpsAt dtaAt tpsSt 
                                  dataExist = tjp `elem` fjpAt
                                  nfjp 
                                    |fst tjp==(-1) = fjpAt
                                    |dataExist = fjpAt
                                    |otherwise = fjpAt ++ [tjp]
                                  ind = fromMaybe (-1) $ elemIndex tjp nfjp
                                  nfco
                                    |sjnAt==ind = selectColor
                                    |fst tjp/=(-1) = linkColor
                                    |otherwise = fontColor
                               in at{fjp=nfjp,fco=nfco}
                         0 -> at{fco=fontColor} 
                         _ -> at
               _    -> at{cnm=""}
      ncnm = if cidAt==0 then "" else cnmAt
   in (natr{cnm=ncnm, cid=cidAt-1} , (if ite natr then "" else ttx, rtx))

breakText :: Text -> (Text,Text)
breakText tx = let (hd,tl) = T.break (=='\n') tx
                   (hda,hdb) = if ' ' `T.elem` hd then T.break (==' ') hd else (hd,T.empty)
                   tl2
                     |tl==T.empty = tl 
                     |T.head tl == '\n' = " "<>tl
                     |otherwise = tl
                in (hda,hdb<>tl2)

dotsToRect :: [Dot] -> [Rect]
dotsToRect dtl = let ds = dotSize
                 in map (\(V2 x y,_) -> V4 (x*ds) (y*ds) ds ds) dtl 

textToJumpData :: FilePos -> TextPos -> Text -> Jump
textToJumpData fpsSt tpsSt ttx = ((fpsSt,T.pack$show fpsSt),(tpsSt,ttx)) 

searchJump :: [Jump] -> [Text] -> TextPos -> FrJp 
searchJump [] _ _ = (-1,(0,0))
searchJump (((fi,fnm),(tp,tn)):xs) dtaAt tpsSt
  | length dtaAt/=2 = (-1,(0,0)) 
  | fnm==head dtaAt && tn==last dtaAt = (tpsSt,(fi,tp))
  | otherwise = searchJump xs dtaAt tpsSt

