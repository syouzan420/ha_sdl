module MyCode(exeCode) where

import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import MyData (State(..),Code)

type Func = [String] -> State -> State

exeCode :: State -> Code -> State
exeCode st cd = let cds = words cd 
                    (arg,funcName) = (init cds, last cds)
                 in fromMaybe idf (lookup funcName funcs) arg st

funcs :: [(String,Func)]
funcs = [("cls",cls)]

idf :: [String] -> State -> State
idf _ st = st

cls :: [String] -> State -> State
cls _ st = st{tex=T.empty,tps=0}
