{-# LANGUAGE OverloadedStrings #-}
module MyData (Pos,Color,Modif(..),State(..),Attr(..),Rubi(..),WMode(..),title,windowSize,initState
              ,fontFiles,imageFiles,fontSize,fontColor,backColor,cursorColor,delayTime
              ,initYokoPos,initTatePos) 
  where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Data.Word (Word8,Word32)

type Pos = V2 CInt
type PointSize = Int
type Color = V4 Word8
data Modif = Alt | Ctr | Shf | Non deriving (Eq, Show) --modifier
data WMode = T | Y deriving (Eq,Show) -- writing mode 

-- tex: edit text
-- atr: text attribute
-- tps: text position
-- ccr: cursor count
-- rbi: for rubi
-- ifm: view formatted text or not
-- icr: cursor appear
data State = State{tex :: !Text, atr :: !Attr, tps :: !Int, crc :: !Int
                  ,ifm :: !Bool, icr :: !Bool}

-- gps: position (x,y) on graphic pixels
-- wmd: writing mode (Tate, Yoko)
-- fsz: font size (appear) (not the original font size)
-- fco: font color
-- ltw: letter width (文字送り)
-- lnw: line width (行送り)
-- wsz: window size (width,height)
data Attr = Attr{gps :: Pos, wmd :: WMode, fsz :: PointSize, fco :: Color
                ,ltw :: CInt, lnw :: CInt, wsz :: V2 CInt, mgn :: V4 CInt
                ,rbi :: Rubi}

-- rps: rubi position
-- rwi: width for rubi
-- rsz: rubi font size
-- irb: is rubi mode?
-- iwr: is writing rubi?
data Rubi = Rubi{rps :: Pos, rwd :: CInt, rsz :: PointSize, irb :: Bool, iwr :: Bool}

title :: T.Text
title = "HA"

windowSize :: V2 CInt
windowSize = V2 480 600

margins :: V4 CInt
margins = V4 20 30 20 30 -- right top left bottom 

initState :: State
initState = State {tex = "これはテストです\n日本語がちゃんと表示されてゐるかな\n長い文章は画面の下とか右までいくと改行されるやうにつくってます\nそして（括弧）とか伸ばし棒「ー」など回転して表示されたり あと 英語なども標準では回転させてゐます\n例へばabcdeとか12345とかね\nIsn't that cool?\nルビのテスト：;rb 椎茸 しいたけ を食べたいな"
                  , atr = initAttr, tps=0, crc=0, ifm=False, icr=False}

initAttr :: Attr
initAttr = Attr{gps = initTatePos, wmd = T, fsz = fontSize, fco = fontColor
               ,ltw = initLetterWidth, lnw = initLineWidth, wsz = windowSize, mgn = margins
               ,rbi = initRubi}

initRubi :: Rubi
initRubi = Rubi{rps = initTatePos, rwd = fromIntegral fontSize, rsz = rubiSize, irb = False, iwr = False}

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map (\s -> "images/"++s++".png") ["onigiri","nori"]

fontSize :: PointSize
fontSize = 24 

rubiSize :: PointSize
rubiSize = 8

initYokoPos :: V2 CInt
initYokoPos = V2 20 30

initTatePos :: V2 CInt
initTatePos = V2 420 30 

initLetterWidth, initLineWidth :: CInt
initLetterWidth = 26; initLineWidth = 30

backColor :: Color
backColor = V4 182 100 255 255

fontColor :: Color 
fontColor = V4 255 255 204 255

cursorColor :: Color
cursorColor = V4 255 255 102 255

delayTime :: Word32
delayTime = 50
