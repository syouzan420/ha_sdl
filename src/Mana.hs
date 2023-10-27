{-# LANGUAGE OverloadedStrings #-}
module Mana where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Tree (Tree(..), Forest(..))
import MyTree (Elm(..),addElem)
  
type Ta = String 
data Yo = Kaz | Moz | Def | Var deriving (Eq, Show) 
data Mn = Mn Ta Yo
type LR = ([Int],[Int])

instance Show Mn where
  show (Mn t y) = t 

makeManas :: T.Text -> Forest Mn
makeManas = makeManas' ([],[]) [] . makeStrings 

getYo :: String -> Yo
getYo x | isMoz x = Moz | isKaz x = Kaz | isDef x = Def | otherwise = Var

makeManas' :: LR -> Forest Mn -> [String] -> Forest Mn 
makeManas' _ mns [] = mns 
makeManas' (pl,pr) mns (x:xs) = 
  let you = getYo x 
      (ls,rs) = if you == Def then getLR x (pl,pr) else (pl,pr)
      (l,ls')
        | null ls = (0,[])
        | otherwise = (head ls,tail ls)
      (r,rs') 
        | x == "(" = (-1,rs)
        | null rs = (0,[])
        | otherwise = (head rs,tail rs)
      mnl = length mns
      tk = min mnl l 
      nl 
        | mnl < l = (l - mnl):ls'
        | x == "(" = (-1):ls'
        | otherwise = ls' 
      nr 
        | you /= Def && r > 0 = if r - 1 == 0 then rs' else (r - 1):rs'
        | r == (-1) = if x==")" then rs' else r:rs
        | otherwise = rs
      nmns = addElem (El (Mn x you) l r) mns 
  in makeManas' (nl,nr) nmns xs  

getLR :: String -> LR -> LR
getLR def (pl,pr) = let names = map (getName . fst) preDef 
                        ind = getIndex def names
                        defws = words$fst$preDef!!ind
                        wsLng = length defws
                        nmInd = getIndex def defws
                    in (nmInd:pl, (wsLng - nmInd - 1):pr) 

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
preDef = [("a x b","a b pro"),("a * b","a b pro"),("(",""),(")","")]

preFunc :: [String] -> String 
preFunc [] = "" 
preFunc ws = 
  case name of
    "sum" -> show $ sum args 
    "pro" -> show $ product args 
    _     -> ""
  where name = last ws
        args = map read (init ws)
