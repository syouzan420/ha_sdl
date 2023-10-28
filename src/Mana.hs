{-# LANGUAGE OverloadedStrings #-}
module Mana where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Tree (Tree(..), Forest(..))
import Data.Maybe (fromMaybe)
import MyTree (Elm(..),addElem)
  
type Ta = String 
data Yo = Kaz | Moz | Io | Def | Spe | Var deriving (Eq, Show) 
data Mn = Mn Ta Yo
type LR = ([Int],[Int])

instance Show Mn where
  show (Mn t y) = t
--    case y of Kaz -> "K"; Moz -> "M"; Io -> "I"; Def -> "D"; Spe -> "S"; Var -> "V"

taiyouMn :: Mn -> (Ta,Yo)
taiyouMn (Mn t y) = (t,y) 

getManaFromTree :: Tree Mn -> Mn
getManaFromTree (Node m _) = m

getManaFromTree' :: Tree Mn -> Mn
getManaFromTree' (Node m []) = m
getManaFromTree' (Node m fm) = makeMana [Node m fm]

searchFromDef :: String -> [(String,String)] -> (String,String) 
searchFromDef _ [] = ("","") 
searchFromDef nm (df:xs) = 
  if name==nm then df else searchFromDef nm xs
    where name = getName (fst df)

defForest :: Forest Mn -> (String,String) 
defForest fm = let mnList = map getManaFromTree fm
                   (taList,yoList) = unzip$map taiyouMn mnList
                   isdef = Def `elem` yoList
                   ind = if isdef then getIndex Def yoList else (-1)
                   name = if isdef then taList!!ind else ""
                   lmnl = length mnList
                in if isdef then searchFromDef name nmlDef else ("","")

evalDef :: Forest Mn -> Mn 
evalDef fm = let (dp,dt) = defForest fm
                 (dpList,dtList) = (words dp, words dt)
                 mnList = map getManaFromTree' fm
                 rmFo = drop (length dpList) fm 
                 (taList,yoList) = unzip$map taiyouMn mnList
                 knv = zip dpList taList
                 evs = map (\x -> fromMaybe x (lookup x knv)) dtList 
                 yo = head$filter (/=Def) yoList
                 rsl = Mn (preFunc evs) yo
              in if null rmFo then rsl else makeMana (Node rsl []:rmFo)

makeMana :: Forest Mn -> Mn
makeMana [] = Mn "" Moz
makeMana [Node x []] = x
makeMana (Node (Mn t0 y0) [] : Node (Mn t1 y1) [] : xs)
  | y0==y1 = case y0 of
      Kaz -> makeMana$Node (Mn (show (read t0 + read t1)) Kaz) []:xs
      Moz -> makeMana$Node (Mn (t0 ++ t1) Moz) [] : xs
  | t0 == ")" = makeMana$Node (Mn t1 y1) [] : xs
  | t1 == ")" = makeMana$Node (Mn t0 y0) [] : xs
  | otherwise = Mn "Error" Moz
makeMana (Node mn [] : Node x y : xs)
  | fst (taiyouMn x) == ")" = makeMana (Node mn y : xs)
  | defForest nfm /= ("","") = makeMana (Node mn [] : Node (evalDef nfm) [] : xs)
  | otherwise = makeMana (Node mn [] : Node (makeMana nfm) [] : xs)
   where nfm = if fst (taiyouMn x) == "(" then [Node (makeMana y) []] else Node x [] : y
makeMana (Node x y : xs) 
  | defForest nfm /= ("","") = makeMana (Node (evalDef nfm) [] : xs)
  | otherwise = makeMana (Node (makeMana nfm) [] : xs)
   where nfm = if fst (taiyouMn x) == "(" then y else Node x [] : y

makeManas :: T.Text -> Forest Mn
makeManas = makeManas' ([],[]) [] . makeStrings 

getYo :: String -> Yo
getYo x | isDef x = Def | isMoz x = Moz | isKaz x = Kaz | isIo x = Io | isSpe x = Spe | otherwise = Var

makeManas' :: LR -> Forest Mn -> [String] -> Forest Mn
makeManas' _ mns [] = mns 
makeManas' (pl,pr) mns (x:xs) = 
  let you = getYo x 
      (ls,rs) = if you == Def || you == Spe then getLR x (pl,pr) else (pl,pr)
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
        | you /= Def && you /= Spe && r > 0 = (r - 1):rs'
        | r == 0 = rs'
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
isDef str = str `elem` map (getName . fst) nmlDef

isSpe :: String -> Bool
isSpe [] = False
isSpe str = str `elem` speDef

isIo :: String -> Bool
isIo [] = False
isIo str = str `elem` map (getName . fst . fst) ioDef

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
preDef = nmlDef ++ map (,"") speDef ++ map fst ioDef

nmlDef :: [(String,String)]
nmlDef = [("a x b","a b pro"),("a * b","a b pro")]

ioDef :: [((String,String),String)]
ioDef = [(("cls","cls"),"()")]

speDef :: [String]
speDef = ["(",")","="]

preFunc :: [String] -> String 
preFunc [] = "" 
preFunc ws = 
  case name of
    "sum" -> show $ sum args 
    "pro" -> show $ product args 
    _     -> ""
  where name = last ws
        args = map read (init ws)
