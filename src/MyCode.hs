{-# Language OverloadedStrings #-}
module MyCode(exeCode) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Linear.V2 (V2(..))
import Linear.V4 (V4(..))
import MyData (State(..),Code,Dt(..),Li(..),Rc(..),Cr(..),Shp(..),Drw(..))

type Func = [String] -> State -> State

exeCode :: State -> Code -> State
exeCode st cd = let cds = words cd 
                    (arg,funcName) = (init cds, last cds)
                    nst = fromMaybe idf (lookup funcName funcs) arg st
                 in if ipr nst then addTex "OK." nst else nst 

addTex :: T.Text -> State -> State
addTex tx st = st{tex = tex st <> "\n" <> tx, tps = tps st + T.length ("\n"<>tx), ipr = False}

showT :: Show a => a -> T.Text
showT = T.pack . show

getMoz :: String -> String
getMoz mz = if length mz > 2 then tail$init mz else mz

funcs :: [(String,Func)]
funcs = [("cls",cls),("color",color),("drawRect",drawRect)]

idf :: [String] -> State -> State
idf _ st = st

cls :: [String] -> State -> State
cls _ st = st{tex=T.empty,tps=0,ipr=False}

color :: [String] -> State -> State
color [x] st = st{cpl=read x}
color _ st = st

drawRect :: [String] -> State -> State
drawRect [a,b,c,d,e] st = 
  let cn = cpl st
      drwSt = drw st
      isFill = getMoz a=="f"
   in st{drw=drwSt++[Drw cn 1 (R (Rc isFill (V2 (read b) (read c)) (V2 (read d) (read e))))]}
drawRect _ st = st
