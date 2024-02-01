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
data Direction = East | North | West | South deriving (Eq, Show) 

-- input mode -- TXT: text mode, PLY: player on maps, BLK: deel with blocks
data IMode = TXT | PLY | BLK deriving (Eq,Show)

-- map property -- Free, Block
data MProp = Fr | Bl deriving (Eq, Show)

--mdi: mode for input
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
--mfn: music file number 
--ims: is music start?
--imp: is music playing?
--pln: player's chara number
--pdr: player's direction
--pac: player animation count
--ipl: whether the player exists
data Waka = Waka {mdi :: !IMode
                 ,set :: ![TSet], tex :: !Text, stx :: !Text, tps :: !Int, scr :: !Pos
                 ,tmd :: !Int, rct :: !Rect, mgn :: !Rect, ltw :: !CInt, lnw :: !CInt
                 ,fsz :: !PointSize, msz :: !Size, tsz :: !CInt
                 ,pps :: !Pos, mps :: !Pos
                 ,gmp :: !GMap, omp :: !OMap, gmr :: !GMProp, omr :: !OMProp
                 ,aco :: !Int
                 ,mfn :: !Int, ims :: !Bool, imp :: !Bool
                 ,pln :: !Int, pdr :: !Direction, pac :: !Int, ipl :: !Bool}

initWaka :: Waka
initWaka = Waka {mdi = TXT
                ,set = [], tex = T.empty, stx = T.empty, tps = 0, scr = V2 0 0
                ,tmd = 0, rct = textRect, mgn = textMgn, ltw = letterWidth, lnw = lineWidth
                ,fsz = fontSize, msz = mapSize0, tsz = initTileSize
                ,pps = initPlayerPos, mps = initMapPos
                ,gmp = initGroundMap, omp = initObjectMap
                ,gmr = initGMapProperty, omr = initOMapProperty
                ,aco = 0
                ,mfn = 0, ims = False, imp = False
                ,pln = 0, pdr = South ,pac = 0, ipl = False} 

mapUpLeftPos :: V2 CInt
mapUpLeftPos = V2 100 10

initPlayerPos :: V2 CInt
initPlayerPos = V2 2 2

plDelay :: Int
plDelay = 5

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

mapSize1 :: Size
mapSize1 = V2 7 5

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

museRoot :: FilePath
museRoot = "./music/"

museFiles :: [FilePath]
museFiles = ["nokorizima","encount"]

