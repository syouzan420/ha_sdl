module MyTree (Elm(..), L, R(..), numR, showF, addElem) where

import Data.Tree 
import Data.List (intercalate)

type L = Int
data R = Ri Int | Rc deriving (Eq, Show)
data Elm a = El a L R 

numR :: R -> Int
numR (Ri i) = i
numR Rc = -1

showT :: Show a => Tree a -> String 
showT (Node x []) = show x
showT (Node x tr) = "["++show x++", "++tail (showF tr)

showF :: Show a => Forest a -> String 
showF [] = ""
showF tr = "[" ++ intercalate ", " (map showT tr) ++ "]"

isLastLeaf :: Int -> Forest a -> Bool
isLastLeaf 0 fo = null fo || (let (Node _ sf) = last fo in null sf)  
isLastLeaf i fo = null fo || (let (Node _ sf) = last fo in isLastLeaf (i-1) sf)

addElem :: Elm a -> Forest a -> Forest a 
addElem (El mn _ _) [] = [Node mn []]
addElem (El mn l r) fo
  | lng >= l && l /= 0
              = let (h,(Node s sf):t) = splitAt (lng - abs l) fo
                    newNode = Node s (t ++ addElem (El mn l r) sf)
                 in h ++ [newNode]
  | numR r > 0 || r == Rc
              = let (it,lt) = (init fo,last fo)
                    Node x subf = lt 
                 in if null subf then fo ++ [Node mn []]
                                 else it ++ [Node x (addElem (El mn l r) subf)]
  | otherwise = let (it,lt) = (init fo,last fo)
                    Node x subf = lt
                 in if isLastLeaf (abs (numR r)) subf then fo ++ [Node mn []] 
                                                else it ++ [Node x (addElem (El mn l r) subf)]
  where lng = length fo

testF = [Node "1" [], Node "2" [], Node "3" [], Node "4" [Node "5" [], Node "6" []], Node "7" []]

