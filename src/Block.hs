module Block where

import Mana (Mn(..),Ta,Yo(..))

type Name = String
type Value = String
data BCell = Core Mn | Hand Mn | Empt deriving (Eq, Show)
type BGrid = [[BCell]]
type GSize = (Int,Int)
type GPos = (Int,Int)
type RPos = (Int,Int)
type Block = (Mn,[RPos])

makeNewGrid :: GSize -> BGrid
makeNewGrid (_,0) = []
makeNewGrid (x,y) = makeXGrid x:makeNewGrid (x,y-1)

makeXGrid :: Int -> [BCell] 
makeXGrid 0 = [] 
makeXGrid x = Empt:makeXGrid (x-1)

getGridSize :: BGrid -> GSize
getGridSize gr = if null gr then (0,0) else (length$head gr,length gr)

getCenter :: GSize -> GPos
getCenter (a,b) = (a `div` 2, b `div` 2)

putFirstBlock :: BGrid -> Block -> BGrid
putFirstBlock gr b = let gsize = getGridSize gr 
                         cpos = getCenter gsize
                      in putBlock gr cpos b

getCell :: BGrid -> GPos -> BCell
getCell gr (p,q) = (gr!!q)!!p

isOnGrid :: GSize -> GPos -> [RPos] -> Bool
isOnGrid _ _ [] = True 
isOnGrid (a,b) (p,q) ((dx,dy):xs)  
  | p+dx >= 0 && p+dx < a && q+dy >= 0 && q+dy < b = isOnGrid (a,b) (p,q) xs
  | otherwise = False

isOffMana :: BGrid -> GPos -> [RPos] -> Bool 
isOffMana _ _ [] = True
isOffMana gr (p,q) ((dx,dy):xs) 
  | cell==Empt = isOffMana gr (p,q) xs
  | otherwise = False
  where cell = getCell gr (p+dx,q+dy) 

isBeHand :: BGrid -> GPos -> Mn -> [RPos] -> Bool
isBeHand gr pos (Mn _ y) [] = let cell = getCell gr pos
                               in case cell of Hand (Mn _ yo) -> y == yo; _ -> False
isBeHand _ _ _ _ = False

canPutBlock :: BGrid -> GPos -> Block -> Bool 
canPutBlock gr pos (mn,rps) = let gsize = getGridSize gr
                                  iOnGrid = isOnGrid gsize pos rps
                               in iOnGrid && (isOffMana gr pos rps || isBeHand gr pos mn rps)

putBlock :: BGrid -> GPos -> Block -> BGrid
putBlock = undefined
