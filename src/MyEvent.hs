module MyEvent (inputEvent) where

import MyData (State(..))
import MySDL.MyInput (myInput)

inputEvent :: State -> IO (State,Bool)
inputEvent st = do
  kc <- myInput
  let isQuit = kc==27 
  return (st,isQuit)
