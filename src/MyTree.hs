module MyTree (Elm(..), showF, addElem) where

import Data.Tree 
import Data.List (intercalate)

data Elm a = El a Int Int

showT :: Show a => Tree a -> String 
showT (Node x []) = show x
showT (Node x tr) = "["++show x++", "++tail (showF tr)

showF :: Show a => Forest a -> String 
showF [] = ""
showF tr = "[" ++ intercalate ", " (map showT tr) ++ "]"

addElem :: Elm a -> Forest a -> Forest a 
addElem (El mn _ _) [] = [Node mn []]
addElem (El mn 0 0) fo = fo ++ [Node mn []]
addElem (El mn l r) fo
  | lng > l && l > 0 = let (h,(Node s sf):t) = splitAt (lng - l) fo
                           newNode = Node s (t++addElem (El mn l r) sf)
                        in h ++ [newNode]
  | r > 0 = let (it,lt) = (init fo,last fo)
                Node x subf = lt  -- last tree
             in if null subf then it ++ [lt, Node mn []]
                             else it ++ [Node x (addElem (El mn l r) subf)]
  | otherwise = fo ++ [Node mn []]
  where lng = length fo

testF = [Node "1" [], Node "2" [], Node "3" [], Node "4" [Node "5" [], Node "6" []], Node "7" []]

