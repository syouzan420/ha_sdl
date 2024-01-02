{-# LANGUAGE OverloadedStrings #-}
module Game.WkData where

import Data.Text (Text)
import qualified Data.Text as T
import Foreign.C.Types (CInt)
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import Data.Word (Word32)

type PointSize = Int
type Pos = V2 CInt
type Mgn = V4 CInt
type Rect = V4 CInt
type TSet = (Text,Text)

data Input = Ri | Up | Lf | Dn | Sp | Rt | Es | No deriving (Eq, Show)

data Waka = Waka {set :: ![TSet], tex :: !Text, tps :: !Int
                 ,rct :: !Rect, mgn :: !Rect, ltw :: !CInt, lnw :: !CInt
                 ,fsz :: !PointSize}

initWaka :: Waka
initWaka = Waka {set = [], tex = T.empty, tps = 0
                ,rct = textRect, mgn = textMgn, ltw = letterWidth, lnw = lineWidth
                ,fsz = fontSize} 


title :: Text
title = "わかひめ"

windowSize :: V2 CInt
windowSize = V2 480 640

textRect :: Rect
textRect = V4 5 320 470 310

textMgn :: Mgn
textMgn = V4 5 5 5 5

letterWidth :: CInt
letterWidth = 26

lineWidth :: CInt
lineWidth = 36

fontSize :: PointSize
fontSize = 24

delayTime :: Word32
delayTime = 60
