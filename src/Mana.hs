{-# LANGUAGE OverloadedStrings #-}
module Mana (makeMana, makeManas, taiyouMn, Mn(..), Ta, Yo(..),Df(..),setDf)  where

import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Tree (Tree(..), Forest)
import Data.Maybe (fromMaybe, isJust)
import MyTree (Elm(..),L,R(..),numR,mtR,ltR,addElem,showF)
import General (getIndex)
  
type Ta = String 
data Yo = Kaz | Moz | Io | Def | Spe | Var deriving (Eq, Show) 
data Mn = Mn Ta Yo deriving Eq
type LR = ([L],[R])
type Definition = ((String,[Yo]),String)
data Dtype = Prim | PrIo | User | UsIo | Non deriving (Eq, Show)
data Df = Df Dtype String [Yo] String deriving (Eq, Show)

instance Show Mn where
  show (Mn t y) = t 
--     ++ "-" ++
--     case y of Kaz -> "K"; Moz -> "M"; Io -> "I"; Def -> "D"; Spe -> "S"; Var -> "V"

taiyouMn :: Mn -> (Ta,Yo)
taiyouMn (Mn t y) = (t,y) 

getManaFromTree :: Tree Mn -> Mn
getManaFromTree (Node m _) = m

getManaFromTree' :: Tree Mn -> Mn
getManaFromTree' (Node m []) = m
getManaFromTree' (Node m fm) = makeMana [Node m fm]

searchFromDef :: String -> [Dtype] -> [[Definition]] -> Maybe Df 
searchFromDef _ _ [] = Nothing 
searchFromDef nm (x:xs) (y:ys) = let dt = searchFromDef' nm y
                                     ((dp,dy),dc) = fromMaybe (("",[]),"") dt
                                  in if dp=="" then searchFromDef nm xs ys else Just (Df x dp dy dc)

searchFromDef' :: String -> [Definition] -> Maybe Definition
searchFromDef' _ [] = Nothing 
searchFromDef' nm (df:xs) =
  if name==nm then Just df else searchFromDef' nm xs
    where name = getName (fst (fst df))

defForest :: Forest Mn -> Maybe Df 
defForest fm = let mnList = map getManaFromTree fm
                   (taList,yoList) = unzip$map taiyouMn mnList
                   isdef = Def `elem` yoList
                   ind = if isdef then getIndex Def yoList else (-1)
                   name = if isdef then taList!!ind else ""
                in if isdef then searchFromDef name [Prim,User,PrIo] [primDef,userDef,prioDef] 
                            else Nothing 

setDf :: String -> Df
setDf name = fromMaybe (Df Non "" [] "") (searchFromDef name [Prim,User,PrIo] [primDef,userDef,prioDef])

evalDef :: Forest Mn -> Mn 
evalDef fm = let Df dt dp dy dc = fromMaybe (Df Non "" [] "") (defForest fm)
                 dpList = words dp
                 dcList = if dt==Prim || dt==PrIo then words dc else (words.T.unpack.addSpaces.T.pack) dc
                 mnList = map getManaFromTree fm
                 rmFo = drop (length dpList) fm 
                 (taList,yoList) = unzip$map taiyouMn mnList
                 yos = zip yoList dy
                 isYoMatch = foldl (\acc (yo,yc) -> acc && (yo==Def || yo==yc)) True yos
                 knv = zip dpList taList
                 evs = if isYoMatch then map (\x -> fromMaybe x (lookup x knv)) dcList else ["yer"]
                 yo = fromMaybe Moz (lookup Def yos)
                 rsl 
                   | dt==Prim = Mn (preFunc evs) yo 
                   | dt==PrIo = Mn (unwords evs) yo
                   | otherwise = makeMana$makeManas (T.pack$unwords evs)
              in if null rmFo then rsl else makeMana (Node rsl []:rmFo)
                 
makeMana :: Forest Mn -> Mn
makeMana [] = Mn "" Moz
makeMana [Node x []] 
  | isJust (defForest [Node x []]) = evalDef [Node x []]
  | otherwise = x 
makeMana (Node (Mn "(" _) y0 : Node (Mn ")" _) y1 : xs) = makeMana (Node (makeMana y0) y1 : xs)
makeMana (Node (Mn t0 y0) [] : Node (Mn t1 y1) [] : xs)
  | y0==y1 = case y0 of
      Kaz -> makeMana$Node (Mn (show (read t0 + read t1)) Kaz) []:xs
      Moz -> makeMana$Node (Mn (init t0 ++ " " ++ tail t1) Moz) [] : xs
  | t0 == ")" = makeMana$Node (Mn t1 y1) [] : xs
  | t1 == ")" = makeMana$Node (Mn t0 y0) [] : xs
  | t0 == "(" = makeMana$Node (Mn t1 y1) [] : xs
  | t1 == "(" = makeMana$Node (Mn t0 y0) [] : xs
  | otherwise = Mn "Error" Spe 
makeMana (Node mn [] : xs) = makeMana [Node mn [], Node (makeMana xs) []]
makeMana (Node x y : xs) 
  | isJust (defForest nfm) = makeMana (Node (evalDef nfm) [] : xs)
  | otherwise = makeMana (Node (makeMana nfm) [] : xs)
   where nfm = if fst (taiyouMn x)=="(" then y else Node x [] : y

makeManas :: T.Text -> Forest Mn
makeManas = makeManas' ([],[]) [] . makeStrings 

getYo :: String -> Yo
getYo x | isDef x = Def | isMoz x = Moz | isKaz x = Kaz | isSpe x = Spe | otherwise = Var

showFLR :: (Forest Mn,LR) -> IO () 
showFLR (fr,lr) = putStrLn (showF fr ++ "\n" ++ show lr)

makeManas' :: LR -> Forest Mn -> [String] -> Forest Mn
makeManas' _ mns [] = mns
makeManas' (pl,pr) mns (x:xs) = 
  let you = getYo x 
      (ls,rs) = if you == Def then getLR x (pl,pr) else (pl,pr)
      (l,ls')
        | null ls = (0,[])
        | otherwise = (head ls,tail ls)
      (r,rs') 
        | null rs = (Ri 0,[])
        | x == ")" = let hr = head rs; ir = numR hr
                      in if ltR hr 1 then 
                          if head (tail rs) == Rc then (Ri (ir-1),tail$tail rs) else (Ri (ir-1),tail rs) 
                                     else (Ri 0, tail rs)
        | otherwise = (head rs,tail rs)
      mnl = length mns
      nl 
        | mnl < l = (l - mnl):ls'
        | x == "(" = (-1):ls'
        | otherwise = if null ls' then ls' else if head ls'==(-1) then 0:ls' else ls'
      nr 
        | you /= Def && you /= Spe && mtR r 0 =
            let ri = numR r - 1 
             in if ri /= 0 then Ri ri:rs' else 
                  if null rs' then Ri 0:rs' 
                              else let hr' = head rs' ; ri' = numR hr'
                                    in if ltR hr' 1 then Ri (ri'-1):tail rs' else Ri ri:rs'
        | x == "(" = if null rs then Rc:rs else if numR r==0 then Rc:rs' else Rc:rs
        | ltR r 1 = rs'
        | r == Rc = r:rs'
        | otherwise = rs
      nmns = addElem (El (Mn x you) l r) mns 
  in makeManas' (nl,nr) nmns xs  

getLR :: String -> LR -> LR
getLR def (pl,pr) = let names = map (getName . fst . fst) preDef
                        ind = getIndex def names
                        defws = words$fst$fst$preDef!!ind
                        wsLng = length defws
                        nmInd = getIndex def defws
                    in (nmInd:pl, Ri (wsLng - nmInd - 1):pr) 

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
isDef str = str `elem` map (getName . fst . fst) preDef

isSpe :: String -> Bool
isSpe [] = False
isSpe str = str `elem` speDef

isIo :: String -> Bool
isIo [] = False
isIo str = str `elem` map (getName . fst . fst) prioDef

makeStrings :: T.Text -> [String]
makeStrings  = words . T.unpack . addSpaces . forMath 

addSpaces :: T.Text -> T.Text
addSpaces txt =
  foldl (\acc nm -> T.replace nm (" "<>nm<>" ") acc) txt (map (T.pack . getName . fst . fst) nameDef)

forMath :: T.Text -> T.Text
forMath = T.replace "+" " " . T.replace "-" " -"

getName :: String -> String
getName def = let ws = words def
                  searchNameList = filter (`notElem` usedForArgs) ws
               in if null searchNameList then "" else head searchNameList

usedForArgs :: [String]
usedForArgs = ["a","b","c","d","e","f","g"]

preDef :: [Definition]
preDef = primDef ++ prioDef ++ userDef

nameDef :: [Definition]
nameDef = primDef ++ map (\x -> ((x,[Spe]),"")) speDef ++ prioDef ++ userDef

primDef :: [Definition]
primDef = [(("a x b",[Kaz, Kaz, Kaz]),"a b pro"),(("a * b",[Kaz, Kaz, Kaz]),"a b pro")]

userDef :: [Definition]
userDef = [(("a bon b",[Kaz, Kaz, Kaz]),"a bxa")]

prioDef :: [Definition]
prioDef = [(("cls",[Io]),"cls")]

speDef :: [String]
speDef = ["(",")","="]

preFunc :: [String] -> String 
preFunc [] = "" 
preFunc ws = 
  case name of
    "pro" -> show $ product args 
    "yer" -> "Yo Error"
    _     -> ""
  where name = last ws
        args = map read (init ws)
