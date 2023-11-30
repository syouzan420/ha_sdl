{-# Language OverloadedStrings #-}
module MyCode(exeCode) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.List.Split (splitOn)
import Linear.V2 (V2(..))
--import Linear.V4 (V4(..))
import MyData (State(..),Code,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..),Img(..))
import Mana (evalCode,taiyouMn,Yo(..),Dtype(..),preDef,userDef)
import MyLib (textIns,lastTps,takeCodes)

type Func = [String] -> State -> State

exeCode :: State -> Code -> State
exeCode st cd = let cds = words cd 
                    (arg,funcName) = (init cds, last cds)
                    nst = fromMaybe idf (lookup funcName funcs) arg st
                 in if ipr nst then addTex "OK." nst else nst 

addTex :: T.Text -> State -> State
addTex tx st = let texSt = tex st; tpsSt = tps st
                   lTps = lastTps tpsSt texSt
                   ntex = textIns ("\n"<>tx) lTps texSt
                   ntps = lTps + T.length ("\n"<>tx)
                in st{tex = ntex, tps = ntps, ipr = False}

--showT :: Show a => a -> T.Text
--showT = T.pack . show

getMoz :: String -> String
getMoz mz = if length mz > 2 then tail$init mz else mz

funcs :: [(String,Func)]
funcs = [("cls",cls),("clear",clear)
        ,("color",color),("lineSize",lineSize)
        ,("drawRect",drawRect),("drawLine",drawLine)
        ,("drawCircle",drawCircle),("drawDot",drawDot),("drawGrid",drawGrid)
        ,("drawImage",drawImage),("run",run),("ha",ha)]

idf :: [String] -> State -> State
idf _ st = st

cls :: [String] -> State -> State
cls _ st = st{tex=T.empty,tps=0,ipr=False}

clear :: [String] -> State -> State
clear _ st = st{drw=[],img=[]}

color :: [String] -> State -> State
color [x] st = st{cpl=read x}
color _ st = st

lineSize :: [String] -> State -> State
lineSize [x] st = st{lsz=read x}
lineSize _ st = st

putDraw :: State -> Shp -> State
putDraw st shp =
  let (cn,sz,drwSt) = (cpl st, lsz st, drw st)
      ndrw = Drw cn sz shp
   in st{drw=drwSt++[ndrw]}

drawRect :: [String] -> State -> State
drawRect [a,b,c,d,e] st = let isFill = getMoz a=="f"
                           in putDraw st (R (Rc isFill (V2 (read b) (read c)) (V2 (read d) (read e))))
drawRect _ st = st

drawLine :: [String] -> State -> State
drawLine [a,b,c,d] st =  putDraw st (L (Li (V2 (read a) (read b)) (V2 (read c) (read d))))
drawLine _ st = st

drawCircle :: [String] -> State -> State
drawCircle [a,b,c,d] st = let isFill = getMoz a=="f"
                           in putDraw st (C (Cr isFill (V2 (read b) (read c)) (read d)))
drawCircle _ st = st  

drawDot :: [String] -> State -> State
drawDot [a,b] st = putDraw st (D (Dt (V2 (read a) (read b))))
drawDot _ st = st

drawGrid :: [String] -> State -> State
drawGrid args st 
  | length args == 6 =
    let [a,b,c,d,e,f] = map read args 
        dw = e `div` a
        dh = f `div` b
        nst = foldl (\acc x -> putDraw acc (L (Li (V2 (c+x) d) (V2 (c+x) (d+f))))) st (map (dw*) [0..a])
     in foldl (\acc y -> putDraw acc (L (Li (V2 c (d+y)) (V2 (c+e) (d+y))))) nst (map (dh*) [0..b])
  | otherwise = st

drawImage :: [String] -> State -> State
drawImage [a,b,c,d,e,f] st = 
  let imgSt = img st
      nimg = Img (V2 (read a) (read b)) (V2 (read c) (read d)) (read e) (getMoz f) 
   in st{img=imgSt++[nimg]}
drawImage _ st = st

run :: [String] -> State -> State
run _ st = let codes = takeCodes (tex st)
               dfnSt = dfn st
               manas = map (taiyouMn.evalCode (preDef++[(User,userDef++dfnSt)])) codes
               ioCodes = map fst $ filter (\(_,y) -> y==Io) manas
               --results = map fst $ filter (\(_,y) -> y/=Io) manas
            in st{cod=ioCodes, msg=["codeExe"]}

strYo :: [(String,Yo)]
strYo = [("k",Kaz),("m",Moz),("i",Io)]

readYo :: String -> Yo
readYo str = fromMaybe (read str) (lookup str strYo)

ha :: [String] -> State -> State
ha [a,b] st =
  let (lfts,rits) = (splitOn "," a, splitOn "," b)
      (tgts,pyos) = break (=="::") lfts
      yos = if null pyos then gessYos tgts (T.pack (unwords rits)) else map readYo (tail pyos)
      ndfn = ((unwords tgts,yos),unwords rits)
   in st{dfn=dfn st++[ndfn]} 
ha _ st = st

gessYos :: [String] -> T.Text -> [Yo]
gessYos lfs rt = undefined

