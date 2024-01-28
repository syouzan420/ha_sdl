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
type Size = V2 CInt
type Mgn = V4 CInt
type Rect = V4 CInt
type TSet = (Text,Text)
type GMap = [String]
type OMap = [String]
type GMProp = [(Pos,MProp)]
type OMProp = [(Pos,MProp)]

data Input = Ri | Up | Lf | Dn | Sp | Rt | Es | No deriving (Eq, Show)

-- map property -- Free, Block
data MProp = Fr | Bl deriving (Eq, Show)

--set: (textIndex,original text)
--tex: whole file text
--stx: showing text (adding with time)
--tps: original text's position 
--scr: scroll in pixels
--tmd: text mode (0:center only one line, 1:normal)
--rct: text field (mode 1)
--mgn: text margin
--ltw: letter width
--lnw: line width
--fsz: font size
--msz: map size
--tsz: tile size
--pps: player position
--mps: map position
--gmp: ground map
--omp: object map
--gmr: ground map property 
--omr: object map property
--aco: animation count
data Waka = Waka {set :: ![TSet], tex :: !Text, stx :: !Text, tps :: !Int, scr :: !Pos
                 ,tmd :: !Int, rct :: !Rect, mgn :: !Rect, ltw :: !CInt, lnw :: !CInt
                 ,fsz :: !PointSize, msz :: !Size, tsz :: !CInt
                 ,pps :: !Pos, mps :: !Pos
                 ,gmp :: !GMap, omp :: !OMap, gmr :: !GMProp, omr :: !OMProp
                 ,aco :: !Int}

initWaka :: Waka
initWaka = Waka {set = [], tex = T.empty, stx = T.empty, tps = 0, scr = V2 0 0
                ,tmd = 0, rct = textRect, mgn = textMgn, ltw = letterWidth, lnw = lineWidth
                ,fsz = fontSize, msz = mapSize0, tsz = initTileSize
                ,pps = initPlayerPos, mps = initMapPos
                ,gmp = initGroundMap, omp = initObjectMap
                ,gmr = initGMapProperty, omr = initOMapProperty
                ,aco = 0} 

mapUpLeftPos :: V2 CInt
mapUpLeftPos = V2 100 10

initPlayerPos :: V2 CInt
initPlayerPos = V2 0 0

initMapPos :: V2 CInt
initMapPos = V2 0 0

initTileSize :: CInt
initTileSize = 32

initGroundMap :: GMap
initGroundMap = ["3413311","3133433","1330100","4342355","3320543"]

initObjectMap :: OMap
initObjectMap = []

initGMapProperty :: GMProp
initGMapProperty = []

initOMapProperty :: OMProp
initOMapProperty = [] 

title :: Text
title = "わかひめ"

windowSize :: V2 CInt
windowSize = V2 480 640

textRect :: Rect
textRect = V4 5 220 470 410

textMgn :: Mgn
textMgn = V4 15 5 5 20 

letterWidth :: CInt
letterWidth = 26

lineWidth :: CInt
lineWidth = 36

fontSize :: PointSize
fontSize = 24

mapSize0 :: Size
mapSize0 = V2 0 0

delayTime :: Word32
delayTime = 80 

mapRoot :: FilePath
mapRoot = "./images/maps/mp"

charaRoot :: FilePath
charaRoot = "./images/charas/ch"

objectRoot :: FilePath
objectRoot = "./images/objects/ob"

enemyRoot :: FilePath
enemyRoot = "./images/enemies/en"

blockRoot :: FilePath
blockRoot = "./images/blocks/bl"
