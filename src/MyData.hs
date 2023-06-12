{-# LANGUAGE OverloadedStrings #-}
module MyData (Pos,Color,State(..),Attr(..),WMode(..),title,windowSize,initState,fontFiles,imageFiles
              ,fontSize,fontColor,backColor,delayTime) 
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
data WMode = T | Y deriving (Eq,Show) -- writing mode 

-- tex: edit text
-- tps: text position
data State = State{tex :: !Text, tps :: !CInt, atr :: !Attr}

-- gps: position (x,y) on graphic pixels
-- wmd: writing mode (Tate, Yoko)
-- fsz: font size (appear) (not the original font size)
-- fco: font color
-- ltw: letter width (文字送り)
-- lnw: line width (行送り)
-- wsz: window size (width,height)
data Attr = Attr{gps :: Pos, wmd :: WMode, fsz :: PointSize, fco :: Color
                ,ltw :: CInt, lnw :: CInt, wsz :: V2 CInt, mgn :: V4 CInt}

title :: T.Text
title = "HA"

windowSize :: V2 CInt
windowSize = V2 480 600

margins :: V4 CInt
margins = V4 20 30 20 30 -- right top left bottom 

initState :: State
initState = State {tex = "日本語はOKですか12345？ それと 英語は横に表示されるのですか？ いやはや 興味深い結果になりますよねーあ", tps = 0, atr = initAttr}

initAttr :: Attr
initAttr = Attr{gps = V2 100 15, wmd = T, fsz = 24, fco = fontColor
               ,ltw = initLetterWidth, lnw = initLineWidth, wsz = windowSize, mgn = margins}

fontFiles :: [FilePath]
fontFiles = map ("font/"++) ["monaco.ttf","marugo.TTC","oshide.otf"]

imageFiles :: [FilePath]
imageFiles = map (\s -> "images/"++s++".png") ["onigiri","nori"]

fontSize :: PointSize
fontSize = 24 

initLetterWidth, initLineWidth :: CInt
initLetterWidth = 26; initLineWidth = 30

backColor :: Color
backColor = V4 182 100 255 255

fontColor :: Color 
fontColor = V4 255 255 204 255

delayTime :: Word32
delayTime = 50
