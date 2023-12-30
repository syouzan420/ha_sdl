{-# LANGUAGE OverloadedStrings #-}
module MyData (Pos,Color,PList,TextPos,TextData,IsFormat,Dot,Code,Jump,FrJp,JBak
              ,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..)
              ,Modif(..),State(..),Active(..),Attr(..),Coding(..),Rubi(..)
              ,WMode(..),EMode(..),FMode(..),Input(..)
              ,title,windowSize,initState,initActive,initAttr,dotSize,imageNames
              ,fontFiles,imageFiles,fontSize,fontColor,backColor,cursorColor,linkColor,selectColor
              ,rubiSize,delayTime,cursorTime
              ,initYokoPos,initTatePos,textFileName,textPosFile,colorPallet,statusPos,dotFileName
              ,textLengthLimit,jumpNameFile) 
  where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Data.Word (Word8,Word32)
import Mana.Mana (Definition)

type TextPos = Int
type Pos = V2 CInt
type Size = V2 CInt
type PointSize = Int
type Color = V4 Word8
type Cnum = Int         -- color number
type PList = ((Bool,Bool),Pos)
type TextData = [(Bool,Text,Attr,[PList])]
type Dot = (Pos,Cnum)
type Code = String
type Name = String
type Jump = ((Int,Text),(Int,Text)) -- ((FileNumber,FileName),(TextPosNumber,TextPosName))
type FrJp = (Int,(Int,Int)) -- (TextPosition, (FilePosNumber,TextPosNumber))
type JBak = (Int,Int) -- (FilePosNumber,TextPosNumber)
type IsFormat = Bool

data Modif = Alt | Ctr | Shf | Non deriving (Eq, Show) --modifier
data WMode = T | Y deriving (Eq,Show) -- writing mode 
data EMode = Nor | Ins deriving (Eq,Show) -- edit mode --Normal Insert
data FMode = Min | Got | Ost deriving (Eq, Ord, Show) -- font mode
data Input = NON | PKY | PMO | NFL | LFL | LRF | JMP | JBK | EXE | QIT deriving (Eq, Show)
-- nothingPressed | isKeyPressed | isMousePressed | isNewFile | isLoadFile | isLoadRecentFile | isJump | isJBack | isExeCode | isQuit

newtype Dt = Dt Pos deriving (Eq,Show)   --Dot: position
data Li = Li Pos Pos deriving (Eq,Show)  --Line : start_position, end_position
data Rc = Rc Bool Pos Size deriving (Eq,Show) --Rectangle: isFill, position,(w,h)
data Cr = Cr Bool Pos CInt deriving (Eq,Show) --Circle: isFill, orgin, radious
data Shp = D Dt | L Li | R Rc | C Cr deriving (Eq,Show) -- Shape
data Drw = Drw Cnum CInt Shp deriving (Eq,Show)

data Img = Img Pos Size CInt Name deriving (Eq,Show) --Image: position, size, rotate, name

-- act: active datas
-- drw: drawing
-- img: images 
-- com: command for normal mode
-- atr: text attribute
-- emd: edit mode (normal or insert) 
-- cpl: color pallet (color number)
-- lsz: line size
-- ifm: view formatted text or not
-- isk: skk editing
-- iup: need text update? (for example: after reading file) 

data State = State{act :: !Active, drw :: ![Drw]
                  ,img :: ![Img], cdn :: !Coding 
                  ,com :: !String , atr :: !Attr
                  ,emd :: !EMode, cpl :: !Cnum, lsz :: !CInt
                  ,ifm :: !Bool, isk :: !Bool, iup :: !Bool}

-- tex: edit text
-- etx: editing text for Kanji-Henkan
-- dts: dots drawing (pixel art)
-- fps: file position
-- tps: text position
-- crc: cursor count
-- icr: cursor appear
data Active = Active{tex :: !Text, etx :: !Text, dts :: ![Dot]
                    ,fps :: !Int, tps :: !Int, crc :: !Int, icr :: !Bool}

-- cod: executable code
-- dfn: definition of Mana
-- msg: message from executed code
-- ipr: 'OK' prompt for code execution
data Coding = Coding{cod :: ![Code], dfn :: ![Definition], msg :: ![String], ipr :: !Bool} 

-- gps: position (x,y) on graphic pixels
-- wmd: writing mode (Tate, Yoko)
-- fsz: font size (appear) (not the original font size)
-- fco: font color
-- ltw: letter width (文字送り)
-- lnw: line width (行送り)
-- wsz: window size (width,height)
-- mgn: margins (right, top, left, bottom)
-- dta: jump data (?)
-- rbi: for rubi
-- jps: jumps (jump target list)
-- fjp: jump from (jump source list)
-- sjn: selected jump number
-- cnm: command name
-- cid: command index
-- fmd: font mode (default Got(hic) mode)
-- ite: is text erase? (don't show text)
data Attr = Attr{gps :: Pos, scr :: Pos, wmd :: WMode, fsz :: PointSize, fco :: Color
                ,ltw :: CInt, lnw :: CInt, wsz :: V2 CInt, mgn :: V4 CInt
                ,dta :: [Text] ,rbi :: Rubi
                ,jps :: [Jump], fjp :: [FrJp], jbk :: [JBak], sjn :: Int
                ,cnm :: Text, cid :: Int
                ,fmd :: FMode, ite :: Bool} deriving (Eq,Show)

-- rps: rubi position
-- rwi: width for rubi
-- tsz: temporal font size
-- tlw: temporal letter width
-- spr: separation from the main font
data Rubi = Rubi{rps :: Pos, rwd :: CInt, tsz :: PointSize, tlw :: CInt, spr :: CInt} deriving (Eq,Show)

title :: T.Text
title = "HA"

--FILENAMES

textFileName :: FilePath
textFileName = "./texts/ha"

dotFileName :: FilePath
dotFileName = "./dots/dot"

textPosFile :: FilePath
textPosFile = "./tpos.txt"

jumpNameFile :: FilePath
jumpNameFile = "./jpnm.txt"

fontFiles :: [FilePath]
fontFiles = map ("fonts/"++) ["ipamjm.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map (\s -> "images/"++s++".png") imageNames 

imageNames :: [String]
imageNames = ["nori","onigiri"] ++ blockNames

blockNames :: [String]
blockNames = map ("block_"++) ["ho","midu","tama","arg"]

-- SIZE AND POSITION

winSizeX, winSizeY :: CInt
winSizeX = 900; winSizeY = 600

windowSize :: V2 CInt
windowSize = V2 winSizeX winSizeY 

fontSize :: PointSize
fontSize = 24 

rubiSize :: PointSize
rubiSize = 10 

dotSize :: CInt
dotSize = 5

initLetterWidth, initLineWidth :: CInt
initLetterWidth = 26; initLineWidth = 36

margins :: V4 CInt
margins = V4 20 30 20 30 -- right top left bottom 

statusPos :: V2 CInt
statusPos = V2 5 5 

initYokoPos :: V2 CInt
initYokoPos = V2 20 30

initTatePos :: V2 CInt
initTatePos = V2 (winSizeX-60) 30 

-- INITIALIZE

initState :: State
initState = State {act = initActive, drw = [], img = []
                  ,cdn = initCoding, com = "", atr = initAttr
                  ,emd=Nor, cpl=1, lsz=1
                  ,ifm=False, isk=False, iup=False}

initActive :: Active
initActive = Active {tex = T.empty, etx = T.empty, dts = [] 
                    ,fps = 0, tps = 0, crc = cursorTime, icr = False}

initCoding :: Coding
initCoding = Coding {cod = [], dfn = [], msg = [], ipr=True}

initAttr :: Attr
initAttr = Attr{gps = initTatePos, scr = V2 0 0, wmd = T, fsz = fontSize, fco = fontColor
               ,ltw = initLetterWidth, lnw = initLineWidth, wsz = windowSize, mgn = margins
               ,dta = [], rbi = initRubi, jps = [], fjp = [], jbk = [], sjn = -1, cnm = "", cid = 0
               ,fmd = Got, ite = False}

initRubi :: Rubi
initRubi = Rubi{rps = initTatePos, rwd = fromIntegral fontSize, tsz = rubiSize, tlw = initLetterWidth
               ,spr = 0}

-- COLOR

backColor :: Color
backColor = V4 182 100 255 255

fontColor :: Color 
fontColor = V4 255 255 204 255

cursorColor :: Color
cursorColor = V4 255 255 102 255

linkColor :: Color
linkColor = V4 102 178 255 255

selectColor :: Color
selectColor = V4 204 255 204 255

blueColor :: Color
blueColor = V4 153 153 255 255

redColor :: Color
redColor = V4 255 102 178 255

colorPallet :: [Color]
colorPallet = [backColor,fontColor,cursorColor,blueColor,redColor,linkColor,selectColor]

-- LIMITS OR DURATION

textLengthLimit :: Int
textLengthLimit = 300

delayTime :: Word32
delayTime = 30 

cursorTime :: Int
cursorTime = 30 
