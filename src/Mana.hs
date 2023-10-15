{-# LANGUAGE OverloadedStrings #-}
module Mana where

import qualified Data.Text as T
import Data.Char (isDigit)
  
type Ta = String 
data Yo = Kaz | Moz | Def | Var deriving (Eq, Show) 
data Mn = Mn Ta Yo

instance Show Mn where
  show (Mn t y) = t++"("++show y++")" 

makeManas :: T.Text -> [[Mn]]
makeManas = makeManas' (0,0) [] . makeStrings 

makeManas' :: (Int, Int) -> [[Mn]] -> [String] -> [[Mn]]
makeManas' _ mns [] = mns 
makeManas' (pl,pr) mns (x:xs) = 
  let you | isMoz x = Moz | isKaz x = Kaz | isDef x = Def | otherwise = Var
      (l,r) = if you == Def then getLR x else (pl,pr)
      mnl = length mns
      tk = min mnl l 
      nl = if mnl < l then l - mnl else 0
      nr = if you /= Def && r > 0 then r - 1 else r
      nmns
        | you == Def && mnl < l = mns++[[Mn x Def]]
        | you == Def = let (up,down) = splitAt (mnl-l) mns in up ++ [concat down ++ [Mn x Def]]
        | r > 0 && not (null mns) = init mns ++ [last mns ++ [Mn x you]]
        | otherwise = mns ++ [[Mn x you]]
  in makeManas' (nl,nr) nmns xs  

getLR :: String -> (Int, Int)
getLR def = let names = map (getName . fst) preDef 
                ind = getIndex def names
                defws = words$fst$preDef!!ind
                wsLng = length defws
                nmInd = getIndex def defws
             in (nmInd, wsLng - nmInd - 1) 

getIndex :: Eq a => a -> [a] -> Int
getIndex _ [] = 0
getIndex t (x:xs) = if t==x then 0 else 1 + getIndex t xs

isMoz :: String -> Bool
isMoz [] = False
isMoz [_] = False 
isMoz (h:tl) = h=='\"' && last tl=='\"'

isKaz :: String -> Bool
isKaz [] = False
isKaz [x] = isDigit x
isKaz (h:tl) = (h=='+' || h=='-' || isDigit h) && all isDigit tl

isDef :: String -> Bool
isDef [] = False
isDef str = str `elem` map (getName . fst) preDef
    

makeStrings :: T.Text -> [String]
makeStrings  = words . T.unpack . addSpaces

addSpaces :: T.Text -> T.Text
addSpaces txt =
  foldl (\acc nm -> T.replace nm (" "<>nm<>" ") acc) txt (map (T.pack . getName . fst) preDef)

getName :: String -> String
getName def = let ws = words def
                  searchNameList = filter (`notElem` usedForArgs) ws
               in if null searchNameList then "" else head searchNameList

usedForArgs :: [String]
usedForArgs = ["a","b","c","d","e","f","g"]

preDef :: [(String,String)]
preDef = [("a x b","a b pro"),("a * b","a b pro")]

preFunc :: [String] -> String 
preFunc [] = "" 
preFunc ws = 
  case name of
    "sum" -> show $ sum args 
    "pro" -> show $ product args 
    _     -> ""
  where name = last ws
        args = map read (init ws)
