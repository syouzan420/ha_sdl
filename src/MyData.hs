{-# LANGUAGE OverloadedStrings #-}
module MyData (Pos,Color,PList,Dot,Dots,Modif(..),State(..),Attr(..),Rubi(..),WMode(..),EMode(..)
              ,title,windowSize,initState,dotSize
              ,fontFiles,imageFiles,fontSize,fontColor,backColor,cursorColor,rubiSize,delayTime
              ,initYokoPos,initTatePos,textFileName,textPosFile,colorPallet,statusPos,dotFileName) 
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
type Cnum = Int         -- color number
type PList = ((Bool,Bool),Pos)
type Dot = (Pos,Cnum)
type Dots = [Dot]

data Modif = Alt | Ctr | Shf | Non deriving (Eq, Show) --modifier
data WMode = T | Y deriving (Eq,Show) -- writing mode 
data EMode = Nor | Ins deriving (Eq,Show) -- edit mode



-- tex: edit text
-- dts: dots drawing (pixel art)
-- atr: text attribute
-- fps: file position
-- tps: text position
-- crc: cursor count
-- emd: edit mode (normal or insert) 
-- cpl: color pallet (color number)
-- ifm: view formatted text or not
-- icr: cursor appear
data State = State{tex :: !Text, dts :: !Dots, atr :: !Attr, fps :: !Int, tps :: !Int
                  ,crc :: !Int, emd :: !EMode, cpl :: !Cnum
                  ,ifm :: !Bool, icr :: !Bool}

-- gps: position (x,y) on graphic pixels
-- wmd: writing mode (Tate, Yoko)
-- fsz: font size (appear) (not the original font size)
-- fco: font color
-- ltw: letter width (文字送り)
-- lnw: line width (行送り)
-- wsz: window size (width,height)
-- mgn: margins (right, top, left, bottom)
-- rbi: for rubi
-- cnm: command name
-- cid: command index
data Attr = Attr{gps :: Pos, scr :: Pos, wmd :: WMode, fsz :: PointSize, fco :: Color
                ,ltw :: CInt, lnw :: CInt, wsz :: V2 CInt, mgn :: V4 CInt
                ,rbi :: Rubi, cnm :: Text, cid :: Int, ios :: Bool} deriving (Eq,Show)

-- rps: rubi position
-- rwi: width for rubi
-- tsz: tempral font size
-- tlw: temporal letter width
-- spr: separation from the main font
data Rubi = Rubi{rps :: Pos, rwd :: CInt, tsz :: PointSize, tlw :: CInt, spr :: CInt} deriving (Eq,Show)

title :: T.Text
title = "HA"

textFileName :: FilePath
textFileName = "./texts/ha"

dotFileName :: FilePath
dotFileName = "./dots/dot"

textPosFile :: FilePath
textPosFile = "./tpos.txt"

winSizeX, winSizeY :: CInt
winSizeX = 900; winSizeY = 600

windowSize :: V2 CInt
windowSize = V2 winSizeX winSizeY 

statusPos :: V2 CInt
statusPos = V2 5 5 

dotSize :: CInt
dotSize = 5

margins :: V4 CInt
margins = V4 20 30 20 30 -- right top left bottom 

initState :: State
initState = State {tex = "", dts = [], atr = initAttr, fps=0, tps=0, crc=0, emd=Nor, cpl=1
                  ,ifm=False, icr=False}

initText = "これはテストです\n日本語がちゃんと表示されてゐるかな\n長い文章は画面の下とか右までいくと改行されるやうにつくってます\nそして（括弧）とか伸ばし棒「ー」など回転して表示されたり あと 英語なども標準では回転させてゐます\n例へばabcdeとか12345とかね\nIsn't that cool?\nルビのテスト：;rb 椎茸 しいたけ を食べたいな"

initAttr :: Attr
initAttr = Attr{gps = initTatePos, scr = V2 0 0, wmd = T, fsz = fontSize, fco = fontColor
               ,ltw = initLetterWidth, lnw = initLineWidth, wsz = windowSize, mgn = margins
               ,rbi = initRubi, cnm = "", cid = 0, ios = False}

initRubi :: Rubi
initRubi = Rubi{rps = initTatePos, rwd = fromIntegral fontSize, tsz = rubiSize, tlw = initLetterWidth
               ,spr = 0}

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map (\s -> "images/"++s++".png") ["onigiri","nori"]

fontSize :: PointSize
fontSize = 24 

rubiSize :: PointSize
rubiSize = 10 

initYokoPos :: V2 CInt
initYokoPos = V2 20 30

initTatePos :: V2 CInt
initTatePos = V2 (winSizeX-60) 30 

initLetterWidth, initLineWidth :: CInt
initLetterWidth = 26; initLineWidth = 36

backColor :: Color
backColor = V4 182 100 255 255

fontColor :: Color 
fontColor = V4 255 255 204 255

cursorColor :: Color
cursorColor = V4 255 255 102 255

delayTime :: Word32
delayTime = 50

colorPallet :: [Color]
colorPallet = [backColor,fontColor,cursorColor,V4 153 153 255 255,V4 255 102 178 255]
